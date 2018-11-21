		#include p18f87k22.inc
	
acs0	udata_acs
temp	     res    1	    ; Temporary storage of difference between current voltage reading and allowed span
upper_limit_u res   1	    ; Upper limit of allowed voltage span <11:8>
upper_limit_l res   1	    ; Upper limit of allowed voltage span <7:0>
lower_limit_u res   1	    ; Lower limit of allowed voltage span <11:8>
lower_limit_l res   1	    ; Lower limit of allowed voltage span <7:0>
	
period_l    res	    1	    ; Lower byte of PWM period
period_u    res	    1	    ; Upper byte of PWM period
counter_l   res	    1	    ; Lower byte of PWM counter
counter_u   res	    1	    ; Upper byte of PWM counter
dc	    res	    1	    ; Software PWM 8-bit duty cycle length
	    
ADC_counter res	1	    ; 8-bit counter to decide time between servo updates
	    
	code
rst	org 0x0
	
	goto	setup
	
	
init_hi	code	0x0008		    ; High-priority interrupt vector
	btfss	INTCON, TMR0IF	    ; If Timer0 has overflowed, then skip the next instruction line
	retfie	FAST		    ; If Timer0 has not overflowed yet, then return from interrupt
	
	incf	counter_l, F	    ; Increment PWM counter. Might give carry
	movlw	0x00
	addwfc	counter_u, F	    ; If carry from increment of counter_l, then add carry to counter_u
	
				    ; *** Compare software PWM counter to DC length and determine if output should be high or low ***
	movf	dc, W		    ; Load W with duty cycle
	subwf	counter_l, W	    ; (counter_l - DC) -> W . Potentially a borrow
	movlw	0x00		    ; 0x00 -> W
	subwfb	counter_u, W	    ; (counter_u - borrow) -> W 
				    ; If counter<DC, difference is negative and we should have N = '1'.
				    ; we then want to output '1' on PWM pin on PORT D
	btfss   STATUS,N			    
	call	output_high	    ; If counter < dc : Output high on PORTD<0>
	btfsc	STATUS, N
	call	output_low	    ; If counter > dc : Output low on PORTD<0> 
	
	movf	counter_l, W
	addlw	0x01		    ; (counter_l + 1) -> W. Potentially a carry
	subwfb	period_l, W	    ; (period_l) - (counter_l + 1) - (C) -> W. Potentially a borrow
	movf	counter_u, W
	subwfb	period_u, W	    ; (period_u) - (counter_u) - (C) -> W
				    ; If period = counter, this difference will be -1 and Status bit N will be set
	btfsc	STATUS, N	
	call	reset_counter	    ; If negative (ie period = counter), then reset counter to zero
	
exit	bcf	INTCON, TMR0IF	    ; Clear interrupt flag
	retfie	FAST		    ; If Timer0 has not overflowed yet, then return fast
	bcf	INTCON, TMR0IF	    ; Clear interrupt flag
	retfie	FAST		    ; Return from interrupt

	
	
	; ******* Set up Flash Program Memory and ADC & PWM functionalities  ************
setup	bcf	EECON1, CFGS	    ; Point to Flash program memory  
	bsf	EECON1, EEPGD	    ; Access Flash program memory
	call	ADC_Setup	    ; Setup ADC
	call	PWM_Setup	    ; Setup software PWM
	
	goto	measure_loop
	
measure_loop
	call	ADC_Read
	movf	upper_limit_l, W    ; (upper_limit_l) -> W
	subwf	ADRESL, W
	movwf	temp		    ; (ADRESL - upper_limit_l) -> (difference_l). Potentially a borrow
	movf	upper_limit_u, W    ; (upper_limit_u) -> W
	subwfb	ADRESH, W	    ; (ADRESH) - (upper_limit_u) - (borrow) -> W

	btfss	STATUS, N
	call	towards_LDR2	    ; If (ADC reading) > (upper limit): Move servo towards LDR2
	btfsc	STATUS, N
	call	check_lower_limit   ; If (ADC reading) < (upper limit): Compare against lower limit

;	; Test that code is working
;	movlw	0x00
;	movwf	TRISC, ACCESS	    ; PORT C all outputs
;	movff	dc, PORTC	    ; Output duty cycle length on PORT C
	
	; Add delay to measurement loop (using reset_counter subroutine, called every 20 ms)
loop	tstfsz	ADC_counter
	bra	loop		    ; While ADC_counter != 0, wait in loop
	movlw	0xA		    ; Reset ADC_counter to .10
	movwf	ADC_counter	    
	
	goto	measure_loop	    ; Stay in measurement loop	
	
check_lower_limit
	movf	lower_limit_l, W    ; (lower_limit_l) -> W
	subwf	ADRESL, W
	movwf	temp		    ; (lower_limit_l - ADRESL) -> (difference_l). Potentially a borrow
	movf	lower_limit_u, W
	subwfb	ADRESH, W	    ; (ADRESH - lower_limit_u - borrow) -> W
	
	btfsc	STATUS, N
	call	towards_LDR1	    ; If (ADC reading) < (lower limit): Move servo towards LDR1
				    ; otherwise, remain in the same position
	return

towards_LDR1
	movlw	0x01
	subwf	dc		    ; DECREMENT (?) duty cycle length by 1
	return
	
	
towards_LDR2
	movlw	0x01
	addwf	dc		    ; INCREMENT (?) duty cycle length by 1
	return

	
	; ******** PWM Settings ***************************************
PWM_Setup
	movlw	0x04
	movwf	period_u
	movlw	0xE2
	movwf	period_l	    ; Set PWM period to 0x04E2 = .1250 counts
	call	reset_counter	    ; Start PWM counter at 0
	movlw	0x7D		    
	movwf	dc		    ; Set PWM duty cycle to 2 ms (=90 degrees)
	
				    ; Output port settings 
	movlw	0xFE		    ; = b'1111 1110' 
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
	return		
	
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
	decf	ADC_counter, F	    ; Decrement counter for motor position update	
	return
	
	
	; ******** ADC Settings ***************************************
ADC_Setup
	bsf	TRISA, RA0	; use pin A0(==AN0) for input
	bsf	ANCON0, ANSEL0  ; set A0 to analog
	movlw   0x01		; select AN0 for measurement
	movwf   ADCON0		; and turn ADC on
	movlw   0x30		; Select 4.096V positive reference
	movwf   ADCON1		; 0V for -ve reference and -ve input
	movlw   0xF6		; Right justified output
	movwf   ADCON2		; Fosc/64 clock and acquisition times
				; *** Set allowed span of voltages for LDR equilibrium ***
	movlw	0x0A
	movwf	upper_limit_u
	movlw	0x8C
	movwf	upper_limit_l	; Upper limit: 2600 mV = 0xA28
	movlw	0x08
	movwf	lower_limit_u
	movlw	0xFC
	movwf	lower_limit_l	; Lower limit: 2400 mV = 0x960
	
	movlw	0x0A
	movwf	ADC_counter	; Initialise ADC_counter
	
	return

ADC_Read
	bsf	ADCON0,GO	    ; Start conversion
adc_loop
	btfsc   ADCON0,GO	    ; Check to see if finished
	bra	adc_loop
	return
	
	
	end