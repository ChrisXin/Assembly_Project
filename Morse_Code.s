;*----------------------------------------------------------------------------
;* Name:    Lab_2_program.s 
;* Purpose: This code template is for Lab 2
;* Author: Eric Praetzel and Rasoul Keshavarzi 
;*----------------------------------------------------------------------------*/
		THUMB 		; Declare THUMB instruction set 
        AREA 		My_code, CODE, READONLY 	; 
		EXPORT 		__MAIN 		; Label __MAIN is used externally q
		ENTRY 
__MAIN
; The following lines are similar to Lab-1 but use an address, in r4, to make it easier.
; Note that one still needs to use the offsets of 0x20 and 0x40 to access the ports
;
; Turn off all LEDs 
		MOV 		R2, #0xC000
		MOV 		R3, #0xB0000000	
		MOV 		R4, #0x0
		MOVT 		R4, #0x2009
		ADD 		R4, R4, R2 		; 0x2009C000 - the base address for dealing with the ports
		STR 		R3, [r4, #0x20]		; Turn off the three LEDs on port 1
		MOV 		R3, #0x0000007C
		STR 		R3, [R4, #0x40] 	; Turn off five LEDs on port 2 

ResetLUT
		LDR         R5, =InputLUT            ; assign R5 to the address at label LUT
		
NextChar
        LDRB        R0, [R5]		; Read a character to convert to Morse
        ADD         R5, #1              ; point to next value for number of delays, jump by 1 byte
		TEQ         R0, #0              ; If we hit 0 (null at end of the string) then reset to the start of lookup table
		BNE			ProcessChar	; If we have a character process it

		MOV			R0, #4		; delay 4 extra spaces (7 total) between words
		BL			DELAY
		BEQ         ResetLUT

ProcessChar	

		BL			CHAR2MORSE	; convert ASCII to Morse pattern in R1		

		MOV			R1, #15
		MOV			R2, #0		;R2 will hold the 'flag'
		MOVT		R6, #0x8000		;store 0...01 in register R6 (the bit mask)
	
LoopBeginning
	
		TEQ 		R1, #0					;compare R1 to 0 and store the condition codes
		BLT			DoneCharacter				;if [R1] < 0 done the letter
		
		ANDS		R7, R6, R8	;do logical AND between the two of them and set the flags
		LSR			R6, #1		;finally left shift R6 (bit mask) for the next iteration
		BEQ 		ZeroBitCase	

OneBitCase
		BL 			LED_ON					;turn off the LED
		MOV			R2, #1		;set the flag to '1'
		B 			PastBitCheck

ZeroBitCase
		TEQ			R2, #0		;compare flag to 0
		BEQ 		FlagZeroCase
	
FlagOneCase
	BL LED_OFF					;turn off the LED
	B PastBitCheck	
	
FlagZeroCase
	SUB				R1, R1, #1	;decrement contents of R1
	B LoopBeginning

PastBitCheck
	
	;delay for one dot
	MOV				R0, #1
	BL DELAY
	
	SUB				R1, R1, #1	;decrment contents of R1
	B LoopBeginning
	
DoneCharacter
	BL LED_OFF
	MOV				R0, #3
	BL DELAY
	B NextChar


;	This is a different way to read the bits in the Morse Code LUT than is in the lab manual.
; 	Choose whichever one you like.
; 
;	First - loop until we have a 1 bit to send  (no code provided)
;
;	This is confusing as we're shifting a 32-bit value left, but the data is ONLY in the lowest 16 bits, so test starting at bit 15 for 1 or 0
;	Then loop thru all of the data bits:
;
		;MOV		R6, #0x8000	; Init R6 with the value for the bit, 15th, which we wish to test
		;LSL		R1, R1, #1	; shift R1 left by 1, store in R1
		;ANDS		R7, R1, R6	; R7 gets R1 AND R6, Zero bit gets set telling us if the bit is 0 or 1
		;BEQ		; branch somewhere it's zero
		;BNE		; branch somewhere - it's not zero

		;....  lots of code
		;B NextChar	;somewhere in your code! 	; This is the end of the main program 

;	Alternate Method #2
; Shifting the data left - makes you walk thru it to the right.  You may find this confusing!
; Instead of shifting data - shift the masking pattern.  Consider this and you may find that
;   there is a much easier way to detect that all data has been dealt with.

;		LSR		R6, #1		; shift the mask 1 bit to the right
;		ANDS		R7, R1, R6	; R7 gets R1 AND R6, Zero bit gets set telling us if the bit is 0 or 1


;	Alternate Method #3
; All of the above methods do not use the shift operation properly.
; In the shift operation the bit which is being lost, or pushed off of the register,
; "falls" into the C flag - then one can BCC (Branch Carry Clear) or BCS (Branch Carry Set)
; This method works very well when coupled with an instruction which counts the number 
;  of leading zeros (CLZ) and a shift left operation to remove those leading zeros.


;	Alternate Method #4
;
; Ok - you are a hot coder and you've got time to burn and want to shorten your code.  Try this:
; Reverse the Morse Code LUT and encode the bits as follows (01 = short, 11 = 3 delay long, 00 = done)
; By doing this one could just shift right and peel off 2 bits at a time, without the need to count to know when you're
; done or peel off a bunch of empty 0's.  This method means that the encoded information ALTERNATES between on and off!
; The first 01 or 11 count is LED on, and the following one is off, then the next is on ....  till you hit 00

;
;	Additional Work
; 
; Are you still bored?  You want to make sweet Morse Code Music?
; Well - lets get the speaker humming.
;
; Note: If you do use this then decrease the delay to 50 to 100ms so that one can both "read" the LED and audio pattern
;
; By modifying your 500ms delay loop to be two loops - an inner loop of 0x200 that toggles the speaker when done
;  and an outer loop to ensure that the total delay is 500ms
;
; The speaker is on Port 0 ping 26 and by toggling it's at an audible frequency one can make a sound
;
; A simple hack to this code is to modify the EOR line to use another register.  If the register is
; 0x4000000 then the speaker will sound; but if it's 0x0 then the speaker stays silent
;

;		LDR	R4, =LED_PORT_ADR	; setup speaker address
;		MOV	R5, #0x4000000		; This is bit 26 which goes to the speaker
;Again		MOV	R3, #0x200
;loopBuzz	MOV	R2, #0x200		; aprox 1kHz since looping 0x10000 times is ~ 10Hz
;loopMore	SUBS	R2, #1			; decreament inner loop to make a sound;
;		BNE	loopMore
;		EOR	R5, #0x4000000		; toggle speaker output
;		STR	R5, [R4]		; write to speaker output
;		SUBS	R3, #1
;		B	Again

; Subroutines
;
;			convert ASCII character to Morse pattern
;			pass ASCII character in R0, output in R1
;			index into MorseLuT must be by steps of 2 bytes
CHAR2MORSE	STMFD		R13!,{R14}	; push Link Register (return address) on stack
			SUB			R0, #0x41
			LDR			R12, =MorseLUT
			LSL			R0, #1
			LDRH		R8, [R12, R0]
			LDMFD		R13!,{R15}	; restore LR to R15 the Program Counter to return


; Turn the LED on, but deal with the stack in a simpler way
; NOTE: This method of returning from subroutine (BX  LR) does NOT work if subroutines are nested!!
;
LED_ON 	   	push 		{r3-r4}		; preserve R3 and R4 on the R13 stack
			MOV			R3, #0xA0000000
			STR			R3, [R4, #0x20]
			pop 		{r3-r4}
			BX 			LR		; branch to the address in the Link Register.  Ie return to the caller

; Turn the LED off, but deal with the stack in the proper way
; the Link register gets pushed onto the stack so that subroutines can be nested
;
LED_OFF	   	STMFD		R13!,{R3, R14}	; push R3 and Link Register (return address) on stack
			MOV			R3, #0xB0000000
			STR			R3, [R4, #0x20]
			LDMFD		R13!,{R3, R15}	; restore R3 and LR to R15 the Program Counter to return

;	Delay 500ms * R0 times
;	Use the delay loop from Lab-1 but loop R0 times around
;
DELAY				STMFD		R13!,{R2, R14}
					MOVT		R11, #0x0004
MultipleDelay		TEQ			R0, #0		; test R0 to see if it's 0 - set Zero flag so you can use BEQ, BNE
					BEQ 		exitDelay
					SUBS		R11, #1
					BNE			MultipleDelay
					MOVT		R11, #0x0007
					SUBS 		R0, #1
					BNE			MultipleDelay	
			
exitDelay		LDMFD		R13!,{R2, R15}

;
; Data used in the program
; DCB is Define Constant Byte size
; DCW is Define Constant Word (16-bit) size
; EQU is EQUate or assign a value.  This takes no memory but instead of typing the same address in many places one can just use an EQU
;
		ALIGN				; make sure things fall on word addresses

; One way to provide a data to convert to Morse code is to use a string in memory.
; Simply read bytes of the string until the NULL or "0" is hit.  This makes it very easy to loop until done.
;
InputLUT	DCB		"EPJXA", 0	; strings must be stored, and read, as BYTES

		ALIGN				; make sure things fall on word addresses
MorseLUT 
		DCW 	0x17, 0x1D5, 0x75D, 0x75 	; A, B, C, D
		DCW 	0x1, 0x15D, 0x1DD, 0x55 	; E, F, G, H
		DCW 	0x5, 0x1777, 0x1D7, 0x175 	; I, J, K, L
		DCW 	0x77, 0x1D, 0x777, 0x5DD 	; M, N, O, P
		DCW 	0x1DD7, 0x5D, 0x15, 0x7 	; Q, R, S, T
		DCW 	0x57, 0x157, 0x177, 0x757 	; U, V, W, X
		DCW 	0x1D77, 0x775 			; Y, Z

; One can also define an address using the EQUate directive
;
LED_PORT_ADR	EQU	0x2009c000	; Base address of the memory that controls I/O like LEDs

		END
Monday 4:51pm

function [t_out, y_out] = euler( f, t_int, y0, n )
    t0 = t_int(1);
    tf = t_int(2);
    t_out = linspace(t0,tf,n);
    h = (tf - t0)/(n - 1);
    y_out = zeros(1,n);
    y_out(1) = y0;
    for k = 1:n-1
        y_out(k + 1) = y_out(k) + h*f(t_out(k), y_out(k));
    end
    
	return;
end
