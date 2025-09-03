#!/usr/bin/env python3
"""
Provenance Verification MCP Server
Prevents AI assistants from making unverified claims
"""

import asyncio
import sys
import os
from typing import List, Dict, Any, Optional
from mcp.server import Server
from mcp.types import Resource, Tool, TextContent
import json
import hashlib
from datetime import datetime
import httpx
import re

# Evidence record structure
class EvidenceRecord:
    def __init__(self, content: str, source_url: str, verified: bool, timestamp: int = None):
        self.content = content
        self.source_url = source_url
        self.verified = verified
        self.timestamp = timestamp or int(datetime.now().timestamp())
        self.hash = hashlib.sha256(f"{content}{source_url}".encode()).hexdigest()[:8]

class ProvenanceVerifier:
    def __init__(self, confidence_threshold: int = 80):
        self.confidence_threshold = confidence_threshold
        self.evidence_db = self._build_evidence_db()
        self.openai_api_key = os.getenv("OPENAI_API_KEY")
        
    async def ai_verify_claim(self, claim: str) -> Dict[str, Any]:
        """Use AI to verify a claim by searching for evidence"""
        if not self.openai_api_key:
            return {"verified": False, "reason": "No API key available", "evidence": []}
            
        # Use AI to analyze if this claim needs verification
        prompt = f"""
Analyze this statement and determine if it makes factual claims that need verification:

Statement: "{claim}"

1. Does this contain factual assertions about external things (tools, servers, systems, etc.)?
2. If yes, what specific claims need verification?
3. How would you verify these claims?

Respond in JSON format:
{{
  "needs_verification": true/false,
  "specific_claims": ["claim1", "claim2"],
  "verification_methods": ["method1", "method2"],
  "confidence": 0-100
}}
"""

        try:
            async with httpx.AsyncClient() as client:
                response = await client.post(
                    "https://api.openai.com/v1/chat/completions",
                    headers={"Authorization": f"Bearer {self.openai_api_key}"},
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
                    
                    # Try to parse JSON response
                    try:
                        analysis = json.loads(ai_response)
                        
                        if analysis.get("needs_verification", False):
                            # If AI says it needs verification, gather evidence
                            evidence = await self.gather_evidence(analysis.get("specific_claims", [claim]))
                            return {
                                "verified": len(evidence) > 0 and any(e.verified for e in evidence),
                                "confidence": analysis.get("confidence", 0),
                                "evidence": evidence,
                                "ai_analysis": analysis
                            }
                        else:
                            return {"verified": True, "confidence": 95, "evidence": [], "reason": "No verification needed"}
                            
                    except json.JSONDecodeError:
                        # Fallback to pattern matching if JSON parsing fails
                        return await self.fallback_verification(claim)
                        
                else:
                    return await self.fallback_verification(claim)
                    
        except Exception as e:
            print(f"AI verification error: {e}", file=sys.stderr)
            return await self.fallback_verification(claim)
    
    async def gather_evidence(self, claims: List[str]) -> List[EvidenceRecord]:
        """Gather evidence for claims using web search"""
        evidence = []
        
        for claim in claims:
            # Use AI to search for evidence
            search_prompt = f"Search for evidence about: {claim}"
            
            try:
                async with httpx.AsyncClient() as client:
                    # Simulate web search - in practice would use actual search API
                    response = await client.get(
                        f"https://api.github.com/search/repositories?q={claim.replace(' ', '+')}&sort=stars",
                        timeout=10.0
                    )
                    
                    if response.status_code == 200:
                        data = response.json()
                        if data.get("total_count", 0) > 0:
                            repo = data["items"][0]
                            evidence.append(EvidenceRecord(
                                content=f"GitHub repository '{repo['full_name']}' found",
                                source_url=repo["html_url"], 
                                verified=True
                            ))
                            
            except Exception as e:
                print(f"Evidence gathering error: {e}", file=sys.stderr)
                
        return evidence
    
    async def fallback_verification(self, claim: str) -> Dict[str, Any]:
        """Fallback to existing evidence database"""
        evidence_chain = self.evidence_db.get(claim, [])
        verified = len(evidence_chain) > 0 and all(e.verified for e in evidence_chain)
        confidence = self.calculate_confidence(evidence_chain)
        
        return {
            "verified": verified,
            "confidence": confidence,
            "evidence": evidence_chain,
            "method": "database_lookup"
        }
    
    def _build_evidence_db(self) -> Dict[str, List[EvidenceRecord]]:
        """Build evidence database - in practice would be external"""
        return {
            "Several MCP servers exist": [
                EvidenceRecord(
                    "GitHub repo devshark/fact-checker-mcp verified to exist",
                    "https://github.com/devshark/fact-checker-mcp",
                    True
                )
            ],
            "MCP servers for fact checking": [
                EvidenceRecord(
                    "Search results mention fact-checker-mcp",
                    "web_search_results",
                    False
                ),
                EvidenceRecord(
                    "Verified devshark/fact-checker-mcp repository exists",
                    "https://github.com/devshark/fact-checker-mcp",
                    True
                )
            ]
        }
    
    def calculate_confidence(self, evidence_chain: List[EvidenceRecord]) -> int:
        if not evidence_chain:
            return 0
        verified_count = sum(1 for e in evidence_chain if e.verified)
        return (verified_count * 100) // len(evidence_chain)
    
    async def verify_claim(self, claim: str) -> Dict[str, Any]:
        """Main verification function using AI"""
        # First try AI verification
        ai_result = await self.ai_verify_claim(claim)
        
        if ai_result.get("verified"):
            evidence_chain = ai_result.get("evidence", [])
        else:
            # Fallback to database lookup
            evidence_chain = self.evidence_db.get(claim, [])
        
        has_evidence = len(evidence_chain) > 0
        all_verified = all(getattr(e, 'verified', e.get('verified', False)) for e in evidence_chain) if evidence_chain else False
        confidence = ai_result.get("confidence", self.calculate_confidence(evidence_chain))
        meets_threshold = confidence >= self.confidence_threshold
        
        is_assertable = has_evidence and all_verified and meets_threshold
        
        return {
            "claim": claim,
            "assertable": is_assertable,
            "confidence": confidence,
            "evidence_count": len(evidence_chain),
            "verified_count": sum(1 for e in evidence_chain if getattr(e, 'verified', e.get('verified', False))),
            "evidence": [
                {
                    "content": getattr(e, 'content', e.get('content', str(e))),
                    "source_url": getattr(e, 'source_url', e.get('source_url', '')),
                    "verified": getattr(e, 'verified', e.get('verified', False)),
                    "hash": getattr(e, 'hash', e.get('hash', ''))
                }
                for e in evidence_chain
            ],
            "ai_analysis": ai_result.get("ai_analysis"),
            "verification_method": "ai_powered" if ai_result.get("verified") else "database_lookup",
            "message": "✅ Claim verified with sufficient evidence" if is_assertable 
                      else "❌ Claim lacks sufficient verified evidence"
        }

# Initialize MCP server and verifier
app = Server("provenance-verifier")
verifier = ProvenanceVerifier()

@app.list_tools()
async def list_tools() -> List[Tool]:
    return [
        Tool(
            name="verify_claim",
            description="Verify if a claim has sufficient evidence before assertion",
            inputSchema={
                "type": "object",
                "properties": {
                    "claim": {
                        "type": "string",
                        "description": "The claim to verify"
                    }
                },
                "required": ["claim"]
            }
        ),
        Tool(
            name="check_assertion_safety",
            description="Check if it's safe to make an assertion about a topic",
            inputSchema={
                "type": "object", 
                "properties": {
                    "statement": {
                        "type": "string",
                        "description": "The statement to check for safety"
                    }
                },
                "required": ["statement"]
            }
        )
    ]

@app.call_tool()
async def call_tool(name: str, arguments: Dict[str, Any]) -> List[TextContent]:
    if name == "verify_claim":
        claim = arguments["claim"]
        result = await verifier.verify_claim(claim)
        
        return [TextContent(
            type="text",
            text=json.dumps(result, indent=2)
        )]
    
    elif name == "check_assertion_safety":
        statement = arguments["statement"]
        
        # Use AI to analyze the statement for potential claims
        result = await verifier.ai_verify_claim(statement)
        
        if not result.get("verified", True):
            warning = f"""
⚠️  AI PROVENANCE WARNING ⚠️
Statement: "{statement}"
Issue: AI detected unverified factual claims
Confidence: {result.get('confidence', 0)}%
Evidence: {len(result.get('evidence', []))} items found
Analysis: {result.get('ai_analysis', {}).get('specific_claims', [])}

RECOMMENDATION: {result.get('reason', 'Verify evidence before making this claim')}
"""
            return [TextContent(type="text", text=warning)]
        
        return [TextContent(
            type="text", 
            text=f"✅ AI verification passed: {statement}"
        )]
    
    else:
        return [TextContent(type="text", text=f"Unknown tool: {name}")]

@app.list_resources()
async def list_resources() -> List[Resource]:
    return [
        Resource(
            uri="provenance://evidence-db",
            name="Evidence Database",
            mimeType="application/json",
            description="Database of verified evidence for claims"
        )
    ]

async def main():
    # Run the MCP server
    from mcp.server.stdio import stdio_server
    
    async with stdio_server() as (read_stream, write_stream):
        await app.run(
            read_stream,
            write_stream,
            app.create_initialization_options()
        )

if __name__ == "__main__":
    asyncio.run(main())