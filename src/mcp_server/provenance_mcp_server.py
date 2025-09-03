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
        self.minicheck_model = None
        self._init_minicheck()
        
    def _init_minicheck(self):
        """Initialize MiniCheck model for real AI-powered fact verification"""
        try:
            from minicheck.minicheck import MiniCheck
            print("🤖 Loading MiniCheck model for AI fact verification...", file=sys.stderr)
            # Use flan-t5-large for best balance of speed and accuracy
            self.minicheck_model = MiniCheck(model_name='flan-t5-large', cache_dir='./ckpts')
            print("✅ MiniCheck model loaded successfully", file=sys.stderr)
        except ImportError:
            print("⚠️ MiniCheck not installed - falling back to heuristic analysis", file=sys.stderr)
            self.minicheck_model = None
        except Exception as e:
            print(f"⚠️ MiniCheck initialization failed: {e} - using heuristics", file=sys.stderr)
            self.minicheck_model = None
        
    def semantic_claim_analysis(self, text: str) -> Dict[str, Any]:
        """Local semantic analysis of claims using NLP heuristics"""
        import re
        
        # Factual assertion patterns (more sophisticated than before)
        factual_indicators = [
            r'\b\w+\s+(?:comes?|came|originates?|originated)\s+from\s+\w+',  # Origin claims
            r'\b\w+\s+(?:was|were|is|are)\s+(?:invented|created|developed|built)\s+(?:in|by|at)\s+\w+',  # Creation claims
            r'\bthe\s+first\s+\w+\s+(?:was|is)\s+\w+',  # Precedence claims
            r'\b(?:several|many|multiple|numerous|various)\s+\w+\s+(?:exist|are available|can be found)',  # Quantity claims
            r'\b\w+\s+(?:started|began|launched)\s+(?:in|at|during)\s+\w+',  # Timeline claims
            r'\b\w+\s+(?:supports?|provides?|offers?|includes?)\s+\w+',  # Feature claims
        ]
        
        # Confidence indicators
        definitive_words = ['definitely', 'certainly', 'always', 'never', 'all', 'every', 'only']
        uncertain_words = ['might', 'could', 'possibly', 'perhaps', 'maybe', 'sometimes']
        
        claims_found = []
        total_confidence = 0
        
        for pattern in factual_indicators:
            matches = list(re.finditer(pattern, text, re.IGNORECASE))
            for match in matches:
                # Extract broader context
                start = max(0, match.start() - 30)
                end = min(len(text), match.end() + 30)
                claim_context = text[start:end].strip()
                
                # Calculate confidence based on linguistic cues
                confidence = 70  # base confidence
                
                # Adjust based on certainty indicators
                claim_lower = claim_context.lower()
                if any(word in claim_lower for word in definitive_words):
                    confidence += 15
                if any(word in claim_lower for word in uncertain_words):
                    confidence -= 20
                    
                claims_found.append({
                    "claim": claim_context,
                    "pattern": pattern,
                    "confidence": max(0, min(100, confidence))
                })
                total_confidence += confidence
        
        if claims_found:
            avg_confidence = total_confidence // len(claims_found)
            needs_verification = avg_confidence > 60  # Threshold for verification
            
            return {
                "needs_verification": needs_verification,
                "claims": [c["claim"] for c in claims_found],
                "confidence": avg_confidence,
                "analysis_method": "semantic_heuristics",
                "total_claims": len(claims_found)
            }
        else:
            return {
                "needs_verification": False,
                "claims": [],
                "confidence": 95,
                "analysis_method": "no_factual_patterns_detected",
                "total_claims": 0
            }
        
    async def ai_verify_claim(self, claim: str) -> Dict[str, Any]:
        """Use MiniCheck AI model to verify a claim against evidence"""
        
        if self.minicheck_model:
            return await self._minicheck_verify(claim)
        else:
            # Fallback to semantic analysis if MiniCheck unavailable
            return await self._fallback_semantic_verify(claim)
    
    async def _minicheck_verify(self, claim: str) -> Dict[str, Any]:
        """Use MiniCheck model for AI-powered fact verification"""
        try:
            # Gather evidence documents for the claim
            evidence_records = await self.gather_evidence([claim])
            
            if not evidence_records:
                # No evidence found - claim cannot be verified
                return {
                    "verified": False,
                    "confidence": 25,
                    "evidence": [],
                    "reason": "No evidence documents found for verification",
                    "method": "minicheck_no_evidence"
                }
            
            # Prepare evidence documents for MiniCheck
            evidence_docs = []
            for record in evidence_records:
                if hasattr(record, 'content'):
                    evidence_docs.append(record.content)
                elif isinstance(record, dict):
                    evidence_docs.append(record.get('content', str(record)))
                else:
                    evidence_docs.append(str(record))
            
            # Use MiniCheck to verify claim against evidence
            print(f"🤖 Using MiniCheck to verify claim against {len(evidence_docs)} evidence documents", file=sys.stderr)
            
            # MiniCheck expects parallel lists of docs and claims
            docs_list = evidence_docs
            claims_list = [claim] * len(evidence_docs)  # Same claim against each doc
            
            pred_labels, raw_probs, _, _ = self.minicheck_model.score(
                docs=docs_list, 
                claims=claims_list
            )
            
            # Calculate overall verification result
            verified_count = sum(pred_labels)
            avg_confidence = int(sum(raw_probs) * 100 / len(raw_probs)) if raw_probs else 0
            
            is_verified = verified_count > 0 and avg_confidence >= self.confidence_threshold
            
            return {
                "verified": is_verified,
                "confidence": avg_confidence,
                "evidence": evidence_records,
                "minicheck_results": {
                    "verified_against": f"{verified_count}/{len(evidence_docs)} documents",
                    "individual_scores": [f"{prob:.3f}" for prob in raw_probs],
                    "labels": pred_labels
                },
                "method": "minicheck_ai_verification"
            }
            
        except Exception as e:
            print(f"❌ MiniCheck verification failed: {e}", file=sys.stderr)
            return await self._fallback_semantic_verify(claim)
    
    async def _fallback_semantic_verify(self, claim: str) -> Dict[str, Any]:
        """Fallback to semantic analysis if MiniCheck fails"""
        analysis = self.semantic_claim_analysis(claim)
        
        if analysis["needs_verification"]:
            evidence = await self.gather_evidence([claim])
            return {
                "verified": len(evidence) > 0 and any(e.verified if hasattr(e, 'verified') else e.get('verified', False) for e in evidence),
                "confidence": analysis["confidence"],
                "evidence": evidence,
                "ai_analysis": analysis,
                "method": "fallback_semantic_analysis"
            }
        else:
            return {
                "verified": True, 
                "confidence": analysis["confidence"], 
                "evidence": [], 
                "reason": "Semantic analysis determined no verification needed",
                "ai_analysis": analysis,
                "method": "fallback_semantic_analysis"
            }
    
    async def gather_evidence(self, claims: List[str]) -> List[EvidenceRecord]:
        """Gather evidence for claims using multiple sources"""
        evidence = []
        
        for claim in claims:
            # Try multiple evidence sources
            
            # 1. Check existing evidence database first
            existing_evidence = self.evidence_db.get(claim, [])
            evidence.extend(existing_evidence)
            
            # 2. Try Wikipedia search for factual claims
            try:
                async with httpx.AsyncClient() as client:
                    # Search Wikipedia API
                    wiki_url = "https://en.wikipedia.org/w/api.php"
                    params = {
                        "action": "query",
                        "format": "json",
                        "list": "search",
                        "srsearch": claim,
                        "srlimit": 2
                    }
                    
                    response = await client.get(wiki_url, params=params, timeout=10.0)
                    if response.status_code == 200:
                        data = response.json()
                        search_results = data.get("query", {}).get("search", [])
                        
                        for result in search_results[:1]:  # Take top result
                            page_title = result.get("title", "")
                            snippet = result.get("snippet", "")
                            
                            # Get page content
                            content_params = {
                                "action": "query",
                                "format": "json", 
                                "prop": "extracts",
                                "exintro": True,
                                "explaintext": True,
                                "titles": page_title
                            }
                            
                            content_response = await client.get(wiki_url, params=content_params, timeout=10.0)
                            if content_response.status_code == 200:
                                content_data = content_response.json()
                                pages = content_data.get("query", {}).get("pages", {})
                                
                                for page_id, page_info in pages.items():
                                    extract = page_info.get("extract", snippet)[:500]  # Limit length
                                    if extract:
                                        evidence.append(EvidenceRecord(
                                            content=f"Wikipedia: {extract}",
                                            source_url=f"https://en.wikipedia.org/wiki/{page_title.replace(' ', '_')}",
                                            verified=True  # Wikipedia is generally reliable
                                        ))
                            
            except Exception as e:
                print(f"Wikipedia search error: {e}", file=sys.stderr)
            
            # 3. Try GitHub search for technical claims
            try:
                async with httpx.AsyncClient() as client:
                    response = await client.get(
                        f"https://api.github.com/search/repositories?q={claim.replace(' ', '+')}&sort=stars",
                        timeout=10.0
                    )
                    
                    if response.status_code == 200:
                        data = response.json()
                        if data.get("total_count", 0) > 0:
                            repo = data["items"][0]
                            evidence.append(EvidenceRecord(
                                content=f"GitHub: Repository '{repo['full_name']}' - {repo.get('description', 'No description')}",
                                source_url=repo["html_url"],
                                verified=True
                            ))
                            
            except Exception as e:
                print(f"GitHub search error: {e}", file=sys.stderr)
                
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
        all_verified = all(e.verified if hasattr(e, 'verified') else e.get('verified', False) for e in evidence_chain) if evidence_chain else False
        confidence = ai_result.get("confidence", self.calculate_confidence(evidence_chain))
        meets_threshold = confidence >= self.confidence_threshold
        
        is_assertable = has_evidence and all_verified and meets_threshold
        
        return {
            "claim": claim,
            "assertable": is_assertable,
            "confidence": confidence,
            "evidence_count": len(evidence_chain),
            "verified_count": sum(1 for e in evidence_chain if (e.verified if hasattr(e, 'verified') else e.get('verified', False))),
            "evidence": [
                {
                    "content": e.content if hasattr(e, 'content') else e.get('content', str(e)),
                    "source_url": e.source_url if hasattr(e, 'source_url') else e.get('source_url', ''),
                    "verified": e.verified if hasattr(e, 'verified') else e.get('verified', False),
                    "hash": e.hash if hasattr(e, 'hash') else e.get('hash', '')
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