#!/usr/bin/env python3
"""Generate comprehensive CI report from test artifacts."""

import argparse
import json
import os
import sys
from pathlib import Path


def collect_artifacts(artifacts_dir):
    """Collect all test artifacts from the artifacts directory."""
    artifacts = {}
    artifacts_path = Path(artifacts_dir)
    
    if not artifacts_path.exists():
        print(f"Artifacts directory {artifacts_dir} not found")
        return artifacts
    
    # Collect different types of artifacts
    for item in artifacts_path.rglob('*'):
        if item.is_file():
            artifact_type = item.parent.name
            if artifact_type not in artifacts:
                artifacts[artifact_type] = []
            artifacts[artifact_type].append(str(item))
    
    print(f"Collected {len(artifacts)} artifact types")
    return artifacts


def generate_summary_report(artifacts, output_dir):
    """Generate summary report from collected artifacts."""
    report = {
        "ci_summary": {
            "total_job_types": len(artifacts),
            "artifacts_collected": sum(len(files) for files in artifacts.values()),
            "status": "COMPLETED"
        },
        "job_results": {},
        "test_coverage": {
            "coq_proofs": "coq-proofs" in artifacts,
            "core_tests": "core-tests" in artifacts,
            "model_tests": any("model" in key for key in artifacts),
            "integration_tests": "integration-tests" in artifacts,
            "security_tests": "security-results" in artifacts
        },
        "recommendations": [
            "Review individual test results for detailed findings",
            "Monitor security test results for vulnerabilities",
            "Validate mathematical proofs for system correctness"
        ]
    }
    
    # Add job-specific results
    for job_type, files in artifacts.items():
        report["job_results"][job_type] = {
            "artifact_count": len(files),
            "status": "ARTIFACTS_COLLECTED"
        }
    
    return report


def write_report(report, output_dir):
    """Write the comprehensive report to output directory."""
    output_path = Path(output_dir)
    output_path.mkdir(exist_ok=True)
    
    # Write JSON report
    json_file = output_path / "ci_summary.json"
    with open(json_file, 'w') as f:
        json.dump(report, f, indent=2)
    
    # Write Markdown summary
    md_file = output_path / "summary.md"
    with open(md_file, 'w') as f:
        f.write("# CI Summary Report\n\n")
        f.write(f"## Overview\n")
        f.write(f"- Total job types: {report['ci_summary']['total_job_types']}\n")
        f.write(f"- Artifacts collected: {report['ci_summary']['artifacts_collected']}\n")
        f.write(f"- Status: {report['ci_summary']['status']}\n\n")
        
        f.write("## Test Coverage\n")
        for test_type, covered in report['test_coverage'].items():
            status = "✅" if covered else "❌"
            f.write(f"- {test_type}: {status}\n")
        
        f.write("\n## Job Results\n")
        for job_type, result in report['job_results'].items():
            f.write(f"- {job_type}: {result['artifact_count']} artifacts\n")
    
    print(f"Reports written to {output_dir}")
    return json_file, md_file


def main():
    """Main function."""
    parser = argparse.ArgumentParser(description="Generate comprehensive CI report")
    parser.add_argument('--artifacts-dir', required=True, help='Directory containing test artifacts')
    parser.add_argument('--output-dir', required=True, help='Directory to write reports')
    
    args = parser.parse_args()
    
    try:
        print(f"Collecting artifacts from {args.artifacts_dir}")
        artifacts = collect_artifacts(args.artifacts_dir)
        
        print("Generating comprehensive report...")
        report = generate_summary_report(artifacts, args.output_dir)
        
        print(f"Writing reports to {args.output_dir}")
        json_file, md_file = write_report(report, args.output_dir)
        
        print(f"✅ CI report generation completed")
        print(f"JSON report: {json_file}")
        print(f"Markdown summary: {md_file}")
        return 0
        
    except Exception as e:
        print(f"❌ Error generating CI report: {e}")
        return 1


if __name__ == "__main__":
    sys.exit(main())