# TECTOR - Transmural EMR Characterization and Tomography via Object Reflectometry

This project consists of two main components:

1. **TI IWR1843BOOST Radar Board Code (C)**: A simple C program that runs on the TI IWR1843BOOST radar board and sends data via UART.
2. **Host Computer Visualization (Ada)**: An Ada program that runs on the host computer, receives data from the radar board via UART, and displays it.

## Prerequisites

### For the TI IWR1843BOOST Board (C Code)

- TI Code Composer Studio (CCS) - Version 11.0.0 or later
- TI mmWave SDK - Version 3.6.0 or later
- TI IWR1843BOOST radar board
- XDS110 debugger (included with the IWR1843BOOST board)

### For the Host Computer (Ada Code)

- GNAT Ada compiler - Version 12.2.0 or later
- Alire package manager
- Windows, Linux, or macOS

## Building the Project

### Building the Ada Host Program

1. Install Alire and GNAT if not already installed.
2. Clone this repository.
3. Run the following commands:

```bash
# Update Alire dependencies
alr update

# Build the Ada program
alr build
```

[TODO]

## License

This project is licensed under the GNU General Public License v3.0 - see the LICENSE file for details.
