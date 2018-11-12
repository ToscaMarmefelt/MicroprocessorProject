	#include p18f87k22.inc
	
	period	res 2		    ; Period of software-generated PWM
	DC	res 2		    ; Duty cycle length
	pwm_counter	res 2	    ; PWM counter
	
	pan	res 2		    ; Reserve 2 bytes in memory for pan duty cycle
	tilt	res 2		    ; Reserve 2 bytes in memory for tilt duty cycle
	
rst	code 0x0000		    ; Reset vector
	
	goto	start
	
init_hi	code	0x0008		    ; High-priority interrupt vector
	btfss	INTCON, TMR0IF	    ; If Timer0 has overflowed, then skip the next instruction line
	retfie	FAST		    ; If Timer0 has not overflowed yet, then return fast
	bcf	INTCON, TMR0IF	    ; Clear interrupt flag
	retfie	FAST		    ; Return from interrupt
	
	; Timer interupt 62.5 kHz
main	code
start	movlw	0xFC		    ; = b'1111 1100' 
	movwf	TRISD, ACCESS	    ; Make PORT D <1:0> outputs
	clrf	LATD, ACCESS	    ; Clear PORT D outputs
	
	movlw	b'1100 1000'	    ; TMR0 on. 8-bit counter, Fcyc = Fosc/4/(prescaler value=1)
	movwf	T0CON, ACCESS	    ; Clockrate = 62.5 kHz
	
	bsf	INTCON, TMR0IE	    ; Enable Timer0 interrupt
	bsf	INTCON, GIE	    ; Enable all interrupts
	
	
	; ******* Test code to output voltage of 4 V
	
;start				; Set PWM period by writing to the PR2 register
	;movlw	0x7C
	;movwf	PR2, ACCESS		; PR2 = 124
				; Set PWM duty cycle 
;	movlw	0x64			; 0x64 = b'0110 0100'
;	movwf	CCPR4L, ACCESS		; Set PWM duty cycle DC<9:2>
;	bcf	CCP4CON, DC4B0, ACCESS	
;	bcf	CCP4CON, DC4B1, ACESS	; Set DC<1:0> = b'00'
;	
;	bcf	TRISG, CCP4, ACCESS	; Make CCP4 pin output
;	
;	bsf	T2CON, T2CKPS1, ACCESS	; TMR2 prescale value 16
;	bsf	T2CON, TMR2ON, ACCESS	; Enable Timer2
;
;	movlw	0x0F			; CCP4CON<3:0> = b'1111' 
;	iorwf	CCP4CON, F, ACCESS	; Configure CCP4 module for PWM operation
;	
;	goto	$
	
	end
