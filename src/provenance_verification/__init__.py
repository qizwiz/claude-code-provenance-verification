"""
Claude Code Provenance Verification System

A real-time AI claim validation system that prevents unverified assertions
through formal mathematical proofs and evidence-based verification.
"""

__version__ = "0.1.0"
__author__ = "Jonathan Hill"
__email__ = "jonathan.f.hill@gmail.com"

from .claim_detector import ClaimDetector, CLAIM_PATTERNS
from .provenance_verifier import ProvenanceVerifier, EvidenceRecord
from .confidence_calculator import ConfidenceCalculator
from .hook_integration import ResponseClaimValidator

__all__ = [
    "ClaimDetector",
    "CLAIM_PATTERNS", 
    "ProvenanceVerifier",
    "EvidenceRecord",
    "ConfidenceCalculator",
    "ResponseClaimValidator",
]