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

async def ai_detect_claims(text: str) -> List[str]:
    """Use AI to detect any factual claims in text that need verification"""
    try:
        # Import and use the AI-powered verifier for claim detection
        import asyncio
        import os
        
        # Add the MCP server path
        current_dir = os.path.dirname(os.path.abspath(__file__))
        mcp_path = os.path.join(current_dir, "..", "mcp_server")
        sys.path.insert(0, mcp_path)
        
        from provenance_mcp_server import ProvenanceVerifier
        
        verifier = ProvenanceVerifier()
        
        # Use semantic analysis to detect claims
        analysis = verifier.semantic_claim_analysis(text)
        
        if analysis["needs_verification"]:
            claims = analysis["claims"]
            print(f"🧠 Semantic analysis detected {len(claims)} factual claims (confidence: {analysis['confidence']}%)", file=sys.stderr)
            return claims
        else:
            print(f"🧠 Semantic analysis found no verifiable claims (method: {analysis['analysis_method']})", file=sys.stderr)
            return []
                    
    except Exception as e:
        print(f"AI claim detection failed: {e}", file=sys.stderr)
    
    return []

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
        print(f"AI verification completely failed: {e}", file=sys.stderr)
        # No fallback - if AI fails, assume safe
        return {"assertable": True, "confidence": 50, "evidence_count": 0}

async def validate_tool_input(tool_data: Dict[str, Any]) -> bool:
    """Validate tool input for unverified claims using pure AI detection"""
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
    
    # Use AI to detect claims
    potential_claims = await ai_detect_claims(text_to_check)
    
    if not potential_claims:
        return True  # No claims detected by AI
    
    print(f"🤖 AI CLAIM VALIDATION: Found {len(potential_claims)} factual claims", file=sys.stderr)
    
    # Verify each AI-detected claim
    blocked_claims = []
    
    for claim in potential_claims:
        verification = call_provenance_verifier(claim)
        
        print(f"   Claim: '{claim[:50]}...'", file=sys.stderr)
        print(f"   Confidence: {verification['confidence']}%", file=sys.stderr)
        print(f"   Evidence: {verification['evidence_count']} items", file=sys.stderr)
        
        if not verification["assertable"]:
            blocked_claims.append(claim)
    
    if blocked_claims:
        print(f"\n❌ AI BLOCKED: {len(blocked_claims)} unverified claims detected:", file=sys.stderr)
        for i, claim in enumerate(blocked_claims, 1):
            print(f"   {i}. {claim[:80]}{'...' if len(claim) > 80 else ''}", file=sys.stderr)
        
        print(f"\n💡 AI RECOMMENDATION: Verify these claims before making assertions", file=sys.stderr)
        print(f"   Use tools like WebFetch to verify external claims", file=sys.stderr)
        print(f"   Provide evidence sources for factual statements", file=sys.stderr)
        
        return False  # Block execution
    
    return True  # Allow execution

async def main():
    """Main hook entry point"""
    try:
        # Read tool data from stdin
        tool_data = json.load(sys.stdin)
        
        # Validate the tool input for claims using AI
        if await validate_tool_input(tool_data):
            print("✅ AI validation passed", file=sys.stderr)
            sys.exit(0)  # Allow execution
        else:
            print("❌ AI validation failed - blocking execution", file=sys.stderr)
            sys.exit(2)  # Block execution with explanation
            
    except json.JSONDecodeError:
        print("Error: Invalid JSON input", file=sys.stderr)
        sys.exit(0)  # Allow on parsing errors (fail-safe)
        
    except Exception as e:
        print(f"Hook error: {e}", file=sys.stderr)
        sys.exit(0)  # Allow on errors (fail-safe)

def run_main():
    """Wrapper to run async main"""
    import asyncio
    asyncio.run(main())

if __name__ == "__main__":
    run_main()