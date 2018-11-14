	#include p18f87k22.inc

acs0	udata_acs
period_l	res 1	    ; Lower byte of PWM period
period_u	res 1	    ; Upper byte of PWM period
dc		res 1	    ; PWM duty cycle length
counter_l	res 1	    ; Lower byte of PWM counter
counter_u	res 1	    ; Upper byte of PWM counter
temp		res 1
	
	;pan	res 2		    ; Reserve 2 bytes in memory for pan duty cycle
	;tilt	res 2		    ; Reserve 2 bytes in memory for tilt duty cycle
	
rst	code 0x0000		    ; Reset vector
	
	goto	start
	
	
	; ******* SOFTWARE PWM SETUP
	
int_hi	code	0x0008		    ; High-priority interrupt vector
	btfss	INTCON, TMR0IF	    ; If Timer0 has overflowed, then skip the next instruction line
	retfie	FAST		    ; If Timer0 has not overflowed yet, then return from interrupt
	
	incf	counter_l, F	    ; Increment PWM counter
	bnc	0x06		    ; IS THIS RIGHT? I want to skip the step of adding carry to counter_u. Or do I even need to do that?
				    ; No need to increment upper byte of PWM counter if no carry from previous line
	movlw	0x00
	addwfc	counter_u, F	    ; If carry from increment of counter_l, then add carry to counter_u
	
	; Compare software PWM counter to DC length and determine if output should be high or low
	movf	dc, W		    ; Load W with  duty cycle
	subwf	counter_l, W	    ; (counter_l - DC) -> W . Potentially a borrow
	movwf	temp		    ; DO I NEED THIS?
	movlw	0x00		    ; 0x00 -> W
	subwfb	counter_u, W	    ; (counter_u - borrow) -> W 
				    ; If counter<DC, difference is negative and we should have N = '1'.
				    ; we then want to output '1' on PWM pin on PORT D
	bnn	0x04		    ; IS THIS RIGHT? If counter>DC, branch to outputLOW ie 4 lines down
	; output HIGH	
	movlw	0x01
	movwf	PORTD, ACCESS	    ; Output '1' on PORT D <0>
	bra	exit		    ; No need to check if counter<period since we know counter<DC(<period)
				    
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
	bra	exit		    ; If counter < period, leave it as it is
	movlw	0x00
	movwf	counter_l	    ; Reset counter = 0
	movwf	counter_u
	
	; Exit interrupt
exit	bcf	INTCON, TMR0IF	    ; Clear interrupt flag
	retfie	FAST		    ; Return from interrupt
	
	
	
	; ******* MAIN PROGRAM
main	code
	
start	; PWM settings
	movlw	0x04
	movwf	period_u
	movlw	0xE2
	movwf	period_l	    ; Set PWM period to 0x04E2 = .1250 counts
	clrf	counter_u
	clrf	counter_l	    ; Start PWM counter at 0
	movlw	0x7D		    
	movwf	dc		    ; Set PWM duty cycle to 2 ms (=90 degrees)
	
	; Output port settings 
	movlw	0xFE		    ; = b'1111 1100' 
	movwf	TRISD, ACCESS	    ; Make PORT D <0> output
	clrf	LATD, ACCESS	    ; Clear PORT D outputs
	
	; Timer settings
	movlw	b'11001000'	    ; TMR0 on. 8-bit counter, Fcyc = Fosc/4/(prescaler value=1)
	movwf	T0CON, ACCESS	    ; Clockrate = 16 MHz
	
	bsf	INTCON, TMR0IE	    ; Enable Timer0 interrupt
	bsf	INTCON, GIE	    ; Enable all interrupts
	
	goto	$		    ; Keep PC here unless directed elsewhere by interrupt
	
	end
