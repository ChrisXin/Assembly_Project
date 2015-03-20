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

; This line is very important in your main program
; Initializes R11 to a 16-bit non-zero value and NOTHING else can write to R11 !!
				MOV			R11, #0xABCD		; Init the random number generator with a non-zero number
	

               ; configure pins on port 2.0 to 2.15 as GPIO
				MOV			R3, #0x0000        
				LDR			R9,=PINSEL4
				STR			R3, [R9]
				
				; read the signals at port 2
				MOV			R3,#0xFBFF	
				LDR			R9,=FIO2MASK
				STR			R3,[R9]
				
				LDR			R5,=FIO2PIN    ; set the address that will be used as Polling Methods in R5	
			
				
loop 			BL 			RandomNum      ; Generate a Random Number as R11 by calling the given subroutine
				MOV			R1, #0x0000		;set R1 as the counter
				MOV			R0, #20000		; Wait for 2 seconds by calling the delay
				BL			DELAY		    ; R0 * 10^-4 Seconds = 2 s
				MOV			R0, R11			; Wait for another period of time( around 0 to 8 seconds) 
				BL			DELAY           ; R11 * 10^-4 Seconds range from 0 to 8 sec
				
				
				MOV			R4, #0x90000000    ;Turn on the LED at P1.29
				STR			R4, [R10, #0x20]
				
			
			
; Start incrementing a register value R1 once every 0.1 millisecond	
increment		LDR			R6,[R5] 	; status register to check whether the user push INT0 
  
			   	CMP			R6, #0      ;Monitor the status of INT0 push button by polling method
				BEQ			stop         ;once the button is pressed, stop incrementing
				
                ; increment my counter for every 0.1 millisecond
				MOV			R0, #1           
				BL 		DELAY         ; call a delay function for 0.1 millisecond		
				ADDS		R1, #0x0001		
								
				MOV			R9, R1          ; let R9 be the temporary storage for R1
				B			increment

				
				;turn off ALL 8 LEDS
stop			MOV 		R3, #0xB0000000		; Turn off three LEDs on port 1  
	        	STR 		R3, [R10, #0x20]
		        MOV 		R3, #0x0000007C
		        STR 		R3, [R10, #0x40] 	; Turn off five LEDs on port 2 	 			
				
				
				MOV			R5, #4             ; we need to logic shift right 8 bits by 4 times!  
				MOV			R1, R9             ;every time when R1 is shifted the most significant 8 bits, we refresh R1.
				
Send_Bit		BL			DISPLAY_NUM      ; send the Least significant 8 bits to the LEDs by calling Display_Num
				
				MOV			R0, #20000		; wait for 2 seconds (then we send the next 8 bits)
				BL			DELAY
				
				LSR			R1, #8            ;logic shift right 8 bits in oder to send the next 8 bits
				SUBS		R5, #1            ;deduce the logic shift times( 4, 3 ,2 ,1)
				BNE			Send_Bit         ;go back to send those bits again until the most significant part completes
				
				MOV			R0, #50000		; wait for 5 seconds
				BL			DELAY           
				B			stop            ; go back to send those 32 bits all over again  

; Display the number in my counter R1 onto the 8 LEDs

