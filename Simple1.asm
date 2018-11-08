	#include p18f87k22.inc
	
	code
	org 0x0
	
	goto	start
	
	; ******* Test code to output voltage of 4 V
	
start				; Set PWM period by writing to the PR2 register
	movlw	0x7C
	movwf	PR2, ACCESS		; PR2 = 124
				; Set PWM duty cycle 
	movlw	0x64			; 0x64 = b'0110 0100'
	movwf	CCPR4L, ACCESS		; Set PWM duty cycle DC<9:2>
	bcf	DC4B0, CCP4CON, ACCESS	
	bcf	DC4B1, CCP4CON, ACESS	; Set DC<1:0> = b'00'
	
	bcf	CCP4, TRISG, ACCESS	; Make CCP4 pin output
	
	bsf	T2CKPS1, T2CON, ACCESS	; TMR2 prescale value 16
	bsf	TMR2ON, T2CON, ACCESS	; Enable Timer2

	movlw	0x0F			; CCP4CON<3:0> = b'1111' 
	iorwf	CCP4CON, F, ACCESS	; Configure CCP4 module for PWM operation
	
	end
