// Header file for input output functions
#include <stdio.h>

// Main function: entry point for execution
int main() {

    // Writing print statement to print hello world
    printf("Hello World");

    return 0;
}

/*

#include <stdint.h>
#include <stddef.h>
#include <string.h>


#include <ti/drivers/uart/UART.h>
#include <ti/control/mmwavelink/mmwavelink.h>
#include <ti/common/sys_common.h>
#include <ti/drivers/pinmux/pinmux.h>

// XWR18xx Platform specific header files 
//#include <ti/drivers/soc/soc.h>
#include <ti/drivers/pinmux/include/pinmux_xwr18xx.h>

// Define some constants 
#define UART_BAUDRATE             115200
#define TEST_MESSAGE              "Hello from TI IWR1843!\r\n"
#define TEST_MESSAGE_INTERVAL_MS  1000 // Send message every 1 second 

// Function prototypes 
void delay_ms(uint32_t ms);

// Global variables 
UART_Handle   uartHandle;


int main(void) {
    // UART parameters
    UART_Params uartParams;

    // Initialize the drivers
    UART_init();

    // Setup the PINMUX to bring out the UART-A pins
    Pinmux_Set_FuncSel(SOC_XWR18XX_PINN5_PADBE, 
                       SOC_XWR18XX_PINN5_PADBE_MSS_UARTA_TX);
    Pinmux_Set_FuncSel(SOC_XWR18XX_PINN4_PADBD, 
                       SOC_XWR18XX_PINN4_PADBD_MSS_UARTA_RX);

    // Initialize the UART parameters
    UART_Params_init(&uartParams);
    uartParams.baudRate       = UART_BAUDRATE;
    uartParams.isPinMuxDone   = 1; // We already set up the pin mux

    // Open the UART instance
    uartHandle = UART_open(0, &uartParams);

    if (uartHandle == NULL) {
        // UART failed to open; hang in an infinite loop
        while (1);
    }

    // Main loop; continuously send the test message
    while (1) {
        // Write the message to UART
        UART_writePolling(
            uartHandle,
            (uint8_t*)TEST_MESSAGE,
            strlen(TEST_MESSAGE)
        );

        // Wait before sending the next message
        delay_ms(TEST_MESSAGE_INTERVAL_MS);
    }
}

void delay_ms(uint32_t ms) {
    volatile uint32_t i, j;
    for (i = 0; i < ms; i++) {
        for (j = 0; j < 5000; j++) {
            // Do nothing; just burn cycles 
        }
    }
}

*/
