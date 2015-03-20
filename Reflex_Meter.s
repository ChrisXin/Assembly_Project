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

DISPLAY_NUM		STMFD		R13!,{R1, R2, R3, R14}
                
                
                ; Dealing with Port 1 (bit 28,29,31)
				MOV			R7, #0x00C0         ; initialize R7 with "1100 0000" to deal with port 28,29
				AND			R7, R7, R1          ;  using and command to check whether my counter (R1)'s first two significant bits are 1
                RBIT		R7, R7				;  reverse R7 register's bit (because direction of my counter and the port bits are opposite!)
				LSL			R7, #4              ;  logical shift left to match the pin 28 and 29
				MOV         R2 , #0x0020        ; initialize R2 with  "0010 0000" to deal with port 31
				AND			R2, R2, R1          ; using and command to check whether my counter (R1)'s third significant bit is 1
				LSL			R2, #26             ; logically shift left by 26 in oder to match bit 31 (p1.31)
  				ORR			R7, R7, R2	        ; combine the bit 28,29 and 31 of values from counter register 1 first three significant bit
				EOR			R7, #0xB0000000		; invert bits 31, 29, & 28 using xor opearation with "1011 0000 0000 0000 0000 0000 0000 0000"
				STR			R7, [R10, #0x20]	; write the bit sequence to the memory for port 1
				
			
				; Dealing with Port 2 (bit 2,3,4,5,6)		
				LSL		    R1, #25             ; logic shift left by 25 bits
                RBIT		R1, R1		        ; reverse R1 register's bit in order to match bit 2.6 to 2.2 from left to right
				EOR			R1, #0x0000007C     ; invert bits 2,3,4,5,6
				STR 		R1, [R10, #0x40]	; write the bit sequence to the memory for port 2 
				
				LDMFD		R13!,{R1, R2, R3, R15}
				

; R11 holds a 16-bit random number via a pseudo-random sequence as per the Linear feedback shift register (Fibonacci) on WikiPedia
; R11 holds a non-zero 16-bit number.  If a zero is fed in the pseudo-random sequence will stay stuck at 0
; Take as many bits of R11 as you need.  If you take the lowest 4 bits then you get a number between 1 and 15.
;   If you take bits 5..1 you'll get a number between 0 and 15 (assuming you right shift by 1 bit).
;
; R11 MUST be initialized to a non-zero 16-bit value at the start of the program OR ELSE!
; R11 can be read anywhere in the code but must only be written to by this subroutine
RandomNum		STMFD		R13!,{R1, R2, R3, R14}

				AND			R1, R11, #0x8000
				AND			R2, R11, #0x2000
				LSL			R2, #2
				EOR			R3, R1, R2
				AND			R1, R11, #0x1000
				LSL			R1, #3
				EOR			R3, R3, R1
				AND			R1, R11, #0x0400
				LSL			R1, #5
				EOR			R3, R3, R1		; the new bit to go into the LSB is present
				LSR			R3, #15
				LSL			R11, #1
				ORR			R11, R11, R3
				
				LDMFD		R13!,{R1, R2, R3, R15}


;		Delay 0.1ms (100us) * R0 times
; 		aim for better than 10% accuracy
DELAY			STMFD		R13!,{R0, R2, R14}

Multiple_Delay
				MOV         R2,#0x84	    ; value for 0.1 sec delay		
				TEQ			R0, #0			; Compare R0 with 0, set a status register
				BEQ 		exitDelay       ; stop delay branch when R0 equals 0 
still_on
				SUBS		R2, #1          ; decrement R2 by 1
				BGT 		still_on        ; loop until R2 is 0
				
				SUBS		R0, #1          ; decrement R0 when finish the previuos 0.1 second delay 
				B	Multiple_Delay		    ; loop continuously 
exitDelay		LDMFD		R13!,{R0, R2, R15}		

LED_BASE_ADR	EQU 	0x2009c000 		; Base address of the memory that controls the LEDs 
PINSEL3			EQU 	0x4002c00c 		; Address of Pin Select Register 3 for P1[31:16]
PINSEL4			EQU 	0x4002c010 		; Address of Pin Select Register 4 for P2[15:0]
FIO2PIN			EQU		0x2009C054      ; Set Address of Fast Input output Pin as a constant 
FIO2MASK		EQU		0x2009C050      ; Set Address of Fast Input output mask as a constant

;FIO0PIN
;	Usefull GPIO(General Purpose IO)Registers
;	FIODIR  - register to set individual pins as input or output
;	FIOPIN  - register to read and write pins
;	FIOSET  - register to set I/O pins to 1 by writing a 1
;	FIOCLR  - register to clr I/O pins to 0 by writing a 1

				ALIGN 

				END 
