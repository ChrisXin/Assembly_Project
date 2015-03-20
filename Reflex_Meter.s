				THUMB 		; Thumb instruction set 
                AREA 		My_code, CODE, READONLY
                EXPORT 		__MAIN
				ENTRY  
__MAIN

; R1 : the counter to be displayed, pass by value
; R5 : stores the address that will be used as Polling Methods
; R6 : status register for polling method (check whether or not the button is pressed)
; INT0 is at p2.10
; LDMFD:		Load Multiple registeres, increment after
; STMFD:		Store Multiple registeres, increment after
; EQU:			Use EQU to define constants. (This is similar to the use of #define to define a constant in C)
; FIOXRegiter:  Fast Input/Output Direction
; BL: Branch with Linked Register ( return to main program after subroutine)


;  Turn off all 8 LEDs 
				LDR			R10, =LED_BASE_ADR		; R10 is a permenant pointer to the base address for the LEDs, offset of 0x20 and 0x40 for the ports

				MOV 		R3, #0xB0000000		; Turn off three LEDs on port 1  
				STR 		R3, [r10, #0x20]
				MOV 		R3, #0x0000007C
				STR 		R3, [R10, #0x40] 	; Turn off five LEDs on port 2 


