
<img src="https://raw.githubusercontent.com/ethereum/vyper/master/logo/vyper-logo-transparent.svg?sanitize=true" alt="" width="110">

[![Build Status](https://circleci.com/gh/ethereum/vyper.svg?style=shield)](https://circleci.com/gh/ethereum/vyper "CircleCI")
[![Documentation Status](https://readthedocs.org/projects/vyper/badge/?version=latest)](http://vyper.readthedocs.io/en/latest/?badge=latest "ReadTheDocs")
[![Coverage Status](https://coveralls.io/repos/github/ethereum/vyper/badge.svg?branch=master)](https://coveralls.io/github/ethereum/vyper?branch=master "Coveralls")
[![PyPI](https://badge.fury.io/py/vyper.svg)](https://pypi.org/project/vyper "PyPI")
[![Docker](https://images.microbadger.com/badges/version/ethereum/vyper.svg)](https://hub.docker.com/r/ethereum/vyper "DockerHub")
[![Snapcraft](https://snapcraft.io/vyper/badge.svg)](https://snapcraft.io/vyper "Snapcraft")
[![Join the chat at https://gitter.im/ethereum/vyper](https://badges.gitter.im/ethereum/vyper.svg)](https://gitter.im/ethereum/vyper?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge "Gitter")


# Getting Started
See [Installing Vyper](http://vyper.readthedocs.io/en/latest/installing-vyper.html) to install vyper.  
See [Tools and Resources](https://github.com/ethereum/vyper/wiki/Vyper-tools-and-resources) for an additional list of framework and tools with vyper support.
See [Documentation](http://vyper.readthedocs.io/en/latest/index.html) for the documentation and overall design goals of the Vyper language.

**Note: Vyper is beta software, use with care**

# Installation
See the [Vyper documentation](https://vyper.readthedocs.io/en/latest/installing-vyper.html)
for build instructions.

# Compiling a contract
To compile a contract, use:
```bash
vyper your_file_name.vy
```

**Alternative for GitHub syntax highlighting: Add a `.gitattributes` file with the line `*.vy linguist-language=Python`**

There is also an [online compiler](https://vyper.online/) available you can use to experiment with
the language and compile to ``bytecode`` and/or ``LLL``.

**Note: While the vyper version of the online compiler is updated on a regular basis it might
be a bit behind the latest version found in the master branch of this repository.**

## Testing (using pytest)
```bash
python setup.py test
```

# Contributing
* See Issues tab, and feel free to submit your own issues
* Add PRs if you discover a solution to an existing issue
* For further discussions and questions talk to us on [gitter](https://gitter.im/ethereum/vyper)
* For more information, see [Contributing](http://vyper.readthedocs.io/en/latest/contributing.html)
