"""Test MCP server integration."""

import sys
import os


def test_mcp_server_basic():
    """Test basic MCP server functionality."""
    print("Testing MCP server basic functionality...")
    
    # Get path relative to repository root
    server_path = os.path.join(
        os.path.dirname(__file__), 
        '../../src/mcp_server/provenance_mcp_server.py'
    )
    server_path = os.path.abspath(server_path)
    
    # For now, just verify the server script exists
    try:
        with open(server_path, 'r') as f:
            content = f.read()
            if 'class ProvenanceVerifier' in content:
                print("✅ MCP server class found")
                return True
            else:
                print("❌ MCP server class not found")
                return False
    except FileNotFoundError:
        print("❌ MCP server file not found")
        return False


def main():
    """Main test function."""
    print("Running MCP server integration tests...")
    
    success = test_mcp_server_basic()
    
    if success:
        print("✅ All MCP server tests passed")
        return 0
    else:
        print("❌ Some MCP server tests failed")
        return 1


if __name__ == "__main__":
    sys.exit(main())