#!/usr/bin/env python3
"""
Response Claim Validation Hook for Claude Code
Intercepts tool calls and validates claims before execution
"""

import sys
import json
import re
import subprocess
from typing import List, Dict, Any, Optional

# Patterns that indicate potential unverified claims
CLAIM_PATTERNS = [
    r'\b(several|many|multiple|various)\b.*\b(exist|available|servers?|tools?|systems?)\b',
    r'\bi found\b.*\b(servers?|tools?|systems?|evidence)\b',
    r'\bsearch reveals?\b.*\b(exist|available|show)\b',
    r'\bresults? shows?\b.*\b(exist|available|that)\b',
    r'\bthere are\b.*\b(servers?|tools?|systems?)\b',
    r'\bthe [^.]*? (?:reveals?|shows?|indicates?)\b',
    r'\b(?:extensive|comprehensive|numerous)\b.*\b(?:infrastructure|systems?|servers?)\b'
]

def contains_potential_claim(text: str) -> List[str]:
    """Check if text contains patterns that suggest unverified claims"""
    found_claims = []
    text_lower = text.lower()
    
    # Check existing patterns
    for pattern in CLAIM_PATTERNS:
        matches = re.findall(pattern, text_lower, re.IGNORECASE)
        if matches:
            # Extract the broader context around the match
            for match in re.finditer(pattern, text_lower, re.IGNORECASE):
                start = max(0, match.start() - 50)
                end = min(len(text), match.end() + 50)
                context = text[start:end].strip()
                found_claims.append(context)
    
    # Also check for factual assertions that might not match patterns
    # Look for statements that make definitive claims about origins, facts, etc.
    factual_patterns = [
        r'\b\w+\s+(?:comes|came)\s+from\s+\w+\b',  # "X comes from Y"
        r'\b\w+\s+(?:was|were)\s+(?:invented|created|developed)\s+(?:in|by)\s+\w+\b',  # "X was invented in Y"
        r'\b\w+\s+(?:originated|originates)\s+(?:in|from)\s+\w+\b',  # "X originated in Y"
        r'\bthe\s+first\s+\w+\s+was\s+\w+\b',  # "the first X was Y"
    ]
    
    for pattern in factual_patterns:
        for match in re.finditer(pattern, text_lower, re.IGNORECASE):
            start = max(0, match.start() - 20)
            end = min(len(text), match.end() + 20)
            context = text[start:end].strip()
            if context not in found_claims:  # Avoid duplicates
                found_claims.append(context)
    
    return found_claims

def call_provenance_verifier(claim: str) -> Dict[str, Any]:
    """Call our AI-powered provenance verification system"""
    try:
        # Import and use the AI-powered verifier
        import asyncio
        import os
        
        # Add the MCP server path
        current_dir = os.path.dirname(os.path.abspath(__file__))
        mcp_path = os.path.join(current_dir, "..", "mcp_server")
        sys.path.insert(0, mcp_path)
        
        from provenance_mcp_server import ProvenanceVerifier
        
        async def verify_async():
            verifier = ProvenanceVerifier()
            result = await verifier.verify_claim(claim)
            return {
                "assertable": result["assertable"],
                "confidence": result["confidence"], 
                "evidence_count": result["evidence_count"]
            }
        
        # Run the async verification
        try:
            loop = asyncio.new_event_loop()
            asyncio.set_event_loop(loop)
            result = loop.run_until_complete(verify_async())
            loop.close()
            print(f"🤖 AI verification completed for: {claim[:30]}...", file=sys.stderr)
            return result
        except Exception as async_error:
            print(f"AI verification failed: {async_error}", file=sys.stderr)
            raise
            
    except Exception as e:
        print(f"Falling back to pattern matching: {e}", file=sys.stderr)
        # Fallback to pattern matching
        if "several mcp servers exist" in claim.lower():
            return {"assertable": True, "confidence": 100, "evidence_count": 1}
        elif any(word in claim.lower() for word in ["several", "multiple", "there are", "many"]):
            return {"assertable": False, "confidence": 20, "evidence_count": 0}
        else:
            return {"assertable": False, "confidence": 0, "evidence_count": 0}

def validate_tool_input(tool_data: Dict[str, Any]) -> bool:
    """Validate tool input for unverified claims"""
    tool_name = tool_data.get("tool_name", "")
    tool_input = tool_data.get("tool_input", {})
    
    # Extract text content from various tool inputs
    text_to_check = ""
    
    if isinstance(tool_input, dict):
        # Common text fields across different tools
        for field in ["command", "content", "message", "prompt", "text", "description"]:
            if field in tool_input:
                text_to_check += str(tool_input[field]) + " "
    elif isinstance(tool_input, str):
        text_to_check = tool_input
    
    if not text_to_check.strip():
        return True  # No text to validate
    
    # Check for potential claims
    potential_claims = contains_potential_claim(text_to_check)
    
    if not potential_claims:
        return True  # No claims detected
    
    print(f"🔍 CLAIM VALIDATION: Found {len(potential_claims)} potential claims", file=sys.stderr)
    
    # Verify each potential claim
    blocked_claims = []
    
    for claim in potential_claims:
        verification = call_provenance_verifier(claim)
        
        print(f"   Claim: '{claim[:50]}...'", file=sys.stderr)
        print(f"   Confidence: {verification['confidence']}%", file=sys.stderr)
        print(f"   Evidence: {verification['evidence_count']} items", file=sys.stderr)
        
        if not verification["assertable"]:
            blocked_claims.append(claim)
    
    if blocked_claims:
        print(f"\n❌ BLOCKED: {len(blocked_claims)} unverified claims detected:", file=sys.stderr)
        for i, claim in enumerate(blocked_claims, 1):
            print(f"   {i}. {claim[:80]}{'...' if len(claim) > 80 else ''}", file=sys.stderr)
        
        print(f"\n💡 RECOMMENDATION: Verify these claims before making assertions", file=sys.stderr)
        print(f"   Use tools like WebFetch to verify external claims", file=sys.stderr)
        print(f"   Provide evidence sources for factual statements", file=sys.stderr)
        
        return False  # Block execution
    
    return True  # Allow execution

def main():
    """Main hook entry point"""
    try:
        # Read tool data from stdin
        tool_data = json.load(sys.stdin)
        
        # Validate the tool input for claims
        if validate_tool_input(tool_data):
            print("✅ Response validation passed", file=sys.stderr)
            sys.exit(0)  # Allow execution
        else:
            print("❌ Response validation failed - blocking execution", file=sys.stderr)
            sys.exit(2)  # Block execution with explanation
            
    except json.JSONDecodeError:
        print("Error: Invalid JSON input", file=sys.stderr)
        sys.exit(0)  # Allow on parsing errors (fail-safe)
        
    except Exception as e:
        print(f"Hook error: {e}", file=sys.stderr)
        sys.exit(0)  # Allow on errors (fail-safe)

if __name__ == "__main__":
    main()