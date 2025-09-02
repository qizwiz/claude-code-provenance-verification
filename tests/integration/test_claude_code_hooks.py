"""Test Claude Code hook integration."""

import sys
import os
import subprocess


def test_hook_installation():
    """Test that the hook is properly installed."""
    print("Testing Claude Code hook installation...")
    
    # Get path relative to repository root
    hook_path = os.path.join(
        os.path.dirname(__file__), 
        '../../src/hooks/response_claim_validator_hook.py'
    )
    hook_path = os.path.abspath(hook_path)
    
    # Basic test that hook script exists and is executable
    try:
        result = subprocess.run([
            'python3', 
            hook_path, 
            '--help'
        ], capture_output=True, text=True, timeout=10)
        
        if result.returncode == 0:
            print("✅ Hook script is executable")
            return True
        else:
            print(f"❌ Hook script failed: {result.stderr}")
            return False
            
    except subprocess.TimeoutExpired:
        print("❌ Hook script timed out")
        return False
    except Exception as e:
        print(f"❌ Hook test failed: {e}")
        return False


def main():
    """Main test function."""
    print("Running Claude Code hook integration tests...")
    
    success = test_hook_installation()
    
    if success:
        print("✅ All hook integration tests passed")
        return 0
    else:
        print("❌ Some hook integration tests failed")
        return 1


if __name__ == "__main__":
    sys.exit(main())