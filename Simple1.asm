	#include p18f87k22.inc
	
	period_l	res 1	    ; Lower byte of PWM period
	period_u	res 1	    ; Upper byte of PWM period
	DC_l		res 1	    ; Lower byte of duty cycle length
	DC_u		res 1	    ; Upper byte of duty cycle length
	counter_l	res 1	    ; Lower byte of PWM counter
	counter_u	res 1	    ; Upper byte of PWM counter
	temp		res 1
	
	;pan	res 2		    ; Reserve 2 bytes in memory for pan duty cycle
	;tilt	res 2		    ; Reserve 2 bytes in memory for tilt duty cycle
	
rst	code 0x0000		    ; Reset vector
	
	goto	start
	
	
	; ******* High-priority interrupt
	;	  Set up software PWM counter
	
int_hi	code	0x0008		    ; High-priority interrupt vector
	btfss	INTCON, TMR0IF	    ; If Timer0 has overflowed, then skip the next instruction line
	retfie	FAST		    ; If Timer0 has not overflowed yet, then return from interrupt
	
	incf	counter_l, F	    ; Increment PWM counter
	bnc	0x03		    ; IS THIS RIGHT? I want to skip the step of adding carry to counter_u. Or do I even need to do that?
				    ; No need to increment upper byte of PWM counter if no carry from previous line
	movlw	0x00
	addwfc	counter_u, F	    ; If carry from increment of counter_l, then add carry to counter_u
	
	; Compare software PWM counter to DC length and determine if output should be high or low
	movf	DC_l, W		    ; Load W with lower byte of duty cycle
	subwf	counter_l, W	    ; (counter_l - DC_l) -> W . Potentially a borrow
	movwf	temp		    ; DO I NEED THIS?
	movf	DC_u, W		    ; DC_u -> W
	subwfb	counter_u, W	    ; (counter_u - DC_u - (C)) -> W 
				    ; If counter<DC, difference is negative and we should have N = '1'.
				    ; we then want to output '1' on PWM pin on PORT D
	bnn	0x04		    ; IS THIS RIGHT? If counter>DC, branch to outputLOW ie 4 lines down
	; output HIGH	
	movlw	0x01
	movwf	PORTD, ACCESS	    ; Output '1' on PORT D <0>
	bra	timer		    ; No need to check if counter<period since we know counter<DC(<period)
				    
	; output LOW 
	movlw	0x00
	movwf	PORTD, ACCESS	    ; Output '0' on PORT D <0> 
	
	; Compare PWM counter to period to see if we need to reset counter
	movf	period_l, W	    
	addlw	0x01		    ; (period_l + 1) -> W
	subwf	counter_l, W	    ; (counter_l - (period_l+1)) -> W . Potentially a borrow
	movf	period_u, W	    ; period_u -> W
	subwfb	counter_u, W	    ; (counter_u - period_u) -> W
	
	bn	0x02		    ; IS THIS RIGHT? If counter = period, we need to reset counter ie move down 2 lines of code
	bra	timer		    ; If counter < period, leave it as it is
	movlw	0x00
	movwf	counter_l	    ; Reset counter = 0
	movwf	counter_u
	
	; Exit interrupt
timer	bcf	INTCON, TMR0IF	    ; Clear interrupt flag
	retfie	FAST		    ; Return from interrupt
	
	
	
	; ******* Timer0 interupt called at frequency 62.5 kHz
main	code
	
start	movlw	0xFE		    ; = b'1111 1100' 
	movwf	TRISD, ACCESS	    ; Make PORT D <0> output
	clrf	LATD, ACCESS	    ; Clear PORT D outputs
	
	movlw	b'1100 1000'	    ; TMR0 on. 8-bit counter, Fcyc = Fosc/4/(prescaler value=1)
	movwf	T0CON, ACCESS	    ; Clockrate = 16 MHz
	
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
