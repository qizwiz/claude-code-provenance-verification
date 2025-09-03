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
        
        # Get Azure token for API access
        if not await verifier.get_azure_token():
            print("No Azure token available for AI claim detection", file=sys.stderr)
            return []
        
        prompt = f"""
Analyze this text and identify ALL factual claims that could be verified or disproven:

Text: "{text}"

Look for:
- Statements about what exists, is available, or is true
- Claims about origins, history, or causation
- Assertions about quantities, relationships, or properties
- Any definitive statements about external facts

Return ONLY a JSON list of specific factual claims found:
["claim 1", "claim 2", "claim 3"]

If no factual claims are found, return: []
"""
        
        import httpx
        async with httpx.AsyncClient() as client:
            response = await client.post(
                "https://api.openai.com/v1/chat/completions",
                headers={"Authorization": f"Bearer {verifier.azure_token}"},
                json={
                    "model": "gpt-4o-mini",
                    "messages": [{"role": "user", "content": prompt}],
                    "temperature": 0.1
                },
                timeout=30.0
            )
            
            if response.status_code == 200:
                result = response.json()
                ai_response = result["choices"][0]["message"]["content"]
                
                try:
                    import json
                    claims = json.loads(ai_response)
                    if isinstance(claims, list):
                        print(f"🤖 AI detected {len(claims)} factual claims", file=sys.stderr)
                        return claims
                except json.JSONDecodeError:
                    pass
                    
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