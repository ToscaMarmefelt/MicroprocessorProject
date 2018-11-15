	#include p18f87k22.inc
	
acs0	udata_acs
period_l	res 1	    ; Lower byte of PWM period
period_u	res 1	    ; Upper byte of PWM period
dc		res 1	    ; PWM duty cycle length
counter_l	res 1	    ; Lower byte of PWM counter
counter_u	res 1	    ; Upper byte of PWM counter
	
rst	code 0x0000		    ; Reset vector
	
	goto	start
	
init_hi	code	0x0008		    ; High-priority interrupt vector
	btfss	INTCON, TMR0IF	    ; If Timer0 has overflowed, then skip the next instruction line
	retfie	FAST		    ; If Timer0 has not overflowed yet, then return from interrupt
	
	;Test that interrupt is working
	;btfss	temp, 0
	;movlw	0x01	
	;btfsc	temp, 0
	;movlw	0x00
	;movwf	temp
	;movff	temp, PORTD
	;End of test
	
	incf	counter_l, F	    ; Increment PWM counter. Might give carry
	movlw	0x00
	addwfc	counter_u, F	    ; If carry from increment of counter_l, then add carry to counter_u
	;movff	counter_u, PORTC
	
	; Compare software PWM counter to DC length and determine if output should be high or low
	movf	dc, W		    ; Load W with  duty cycle
	subwf	counter_l, W	    ; (counter_l - DC) -> W . Potentially a borrow
	movlw	0x00		    ; 0x00 -> W
	subwfb	counter_u, W	    ; (counter_u - borrow) -> W 
	;movwf	PORTC, ACCESS
				    ; If counter<DC, difference is negative and we should have N = '1'.
				    ; we then want to output '1' on PWM pin on PORT D
	btfss   STATUS,N			    
	call	output_high	    ; If counter > dc : Output low on PORTD<0>
	btfsc	STATUS, N
	call	output_low	    ; If counter < dc : Output high on PORTD<0> 
	;bra	exit		    ; No need to check if counter<period since we know counter<DC(<period)

;	; Compare PWM counter to period to see if we need to reset counter
;	movf	period_l, W	    
;	addlw	0x01		    ; (period_l + 1) -> W
;	subwf	counter_l, W	    ; (counter_l - (period_l+1)) -> W . Potentially a borrow
;	movf	period_u, W	    ; period_u -> W
;	subwfb	counter_u, W	    ; (counter_u - period_u) -> W
	
	movf	counter_l, W
	addlw	0x01		    ; (counter_l + 1) -> W. Potentially a carry
	subwfb	period_l, W	    ; (period_l) - (counter_l + 1) - (C) -> W. Potentially a borrow
	movf	counter_u, W
	subwfb	period_u, W	    ; (period_u) - (counter_u) - (C) -> W
				    ; If period = counter, this difference will be -1 and Status bit N will be set
	
	btfsc	STATUS, N	
	call	reset_counter	    ; If negative (ie period = counter), then reset counter to zero
	
;	bn	0x02		    ; IS THIS RIGHT? If counter = period, we need to reset counter ie move down 2 lines of code
;	bra	exit		    ; If counter < period, leave it as it is
;	movlw	0x00
;	movwf	counter_l	    ; Reset counter = 0
;	movwf	counter_u
	
	; Exit interrupt
exit	bcf	INTCON, TMR0IF	    ; Clear interrupt flag
	retfie	FAST		    ; If Timer0 has not overflowed yet, then return fast
	bcf	INTCON, TMR0IF	    ; Clear interrupt flag
	retfie	FAST		    ; Return from interrupt

	
start	; PWM settings
	movlw	0x04
	movwf	period_u
	movlw	0xE2
	movwf	period_l	    ; Set PWM period to 0x04E2 = .1250 counts
	call	reset_counter	    ; Start PWM counter at 0
	movlw	0x7D		    
	movwf	dc		    ; Set PWM duty cycle to 2 ms (=90 degrees)
	
	;movlw	0x00
	;movwf	temp		    ; test interrupt
	
	
	; Output port settings 
	movlw	0xFE		    ; = b'1111 1100' 
	movwf	TRISD, ACCESS	    ; Make PORT D <0> output
	movlw	0x00		    ; Port C for testing only
	movwf	TRISC, ACCESS
	
	movlw	0xFC		    ; = b'1111 1100' 
	movwf	TRISD, ACCESS	    ; Make PORT D <1:0> outputs
	clrf	LATD, ACCESS	    ; Clear PORT D outputs
	
	movlw	b'11001000'	    ; TMR0 on. 8-bit counter, Fcyc = Fosc/4/(prescaler value=1)
	movwf	T0CON, ACCESS	    ; Clockrate = 62.5 kHz
	
	bsf	INTCON, TMR0IE	    ; Enable Timer0 interrupt
	bsf	INTCON, GIE	    ; Enable all interrupts
	
	goto	$
	
	
output_low
	movlw	0x01
	iorwf	PORTD, F	    ; Make PORTD<0>='1' and <7:1> remain the same. Output to PORTD
	return
output_high 
	movlw	0xFE
	andwf	PORTD, F	    ; Make PORTD<0>='0' and <7:1> remain the same. Output to PORTD
	return
reset_counter
	clrf	counter_l
	clrf	counter_u
	return
	
	
	end
