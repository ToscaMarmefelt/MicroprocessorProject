		#include p18f87k22.inc
	
acs0	udata_acs
beforeH	    res 1   ; Reserve 8 bits for storage of ADC D<11:8> from previous measurement
beforeL	    res	1   ; Reserve 8 bits for storage of ADC D<7:0> from previous measurement
nowH	    res	1   ; Reserve 8 bits for storage of ADC D<11:8> from current measurement	    
nowL	    res	1   ; Reserve 8 bits for storage of ADC D<7:0> from current measurement
differenceH res	1   ; Temporary storage of difference between now and before voltages
differenceL res	1   ; Temporary storage of difference between now and before voltages

period_l    res 1	    ; Lower byte of PWM period
period_u    res 1	    ; Upper byte of PWM period
counter_l   res 1	    ; Lower byte of PWM counter
counter_u   res 1	    ; Upper byte of PWM counter
dc	    res	1   ; Software PWM 8-bit duty cycle length
delay_counter	res 1 ; Counter register for test delay
	    
	code
rst	org 0x0
	
	goto	setup
	
	
init_hi	code	0x0008		    ; High-priority interrupt vector
	btfss	INTCON, TMR0IF	    ; If Timer0 has overflowed, then skip the next instruction line
	retfie	FAST		    ; If Timer0 has not overflowed yet, then return from interrupt
	
	incf	counter_l, F	    ; Increment PWM counter. Might give carry
	movlw	0x00
	addwfc	counter_u, F	    ; If carry from increment of counter_l, then add carry to counter_u
	
	; Compare software PWM counter to DC length and determine if output should be high or low
	movf	dc, W		    ; Load W with  duty cycle
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
setup	bcf	EECON1, CFGS	; Point to Flash program memory  
	bsf	EECON1, EEPGD 	; Access Flash program memory
	call	ADC_Setup	; Setup ADC
	call	PWM_Setup	; Setup software PWM
	
	goto	measure_loop
	
measure_loop
	call	ADC_Read	
	movff	ADRESH, nowH	
	movff	ADRESL, nowL	; Load ADC output into nowH/L file registers
	movf	beforeL, W	; (beforeL) -> W
	subwf	nowL, W		; Subtract low byte of successive ADC readings
	movwf	differenceL	; (nowL - beforeL) -> differenceL. Potentially a borrow
	movf	beforeH, W	; (beforeH) -> W
	subwfb	nowH, W		; Subtract upper byte of successive ADC readings
	movwf	differenceH	; (nowH - beforeH - borrow) -> differenceH
				; Depending on the sign of (difference):
	btfsc	STATUS, N	
	call	diff_negative	; If (difference) is negative, execute this line
	btfss	STATUS, N	
	call	diff_positive	; If (difference) is positive, execute this line
	
	
	; Test that code is working
	movlw	0x00
	movwf	TRISC, ACCESS	; PORT C all outputs
	movff	dc, PORTC	; Output duty cycle length on PORT C
	;call	test_delay	; Delay for x s so we can see duty cycle LEDs on PORT C
	
	
	goto	measure_loop	; Stay in measurement loop	
	
	
diff_negative			; Any abs(difference) smaller than 100 mV will be considered negligable 
	movlw	0x64		; 0x64 = 100 mV with our ADC settings
	addwf	differenceL
	movlw	0x00
	addwfc	differenceH	; (difference) + 100 mV
	btfsc	STATUS, N
	call	towards_LDR1	; If (difference + 100mV) < 0 then servo will move one "step" & (before) registers are updated
				; If (difference + 100mV) > 0 then no movement of servo
	return
	
towards_LDR1
	movlw	0x01
	addwf	dc		; INCREMENT (?) duty cycle length by 1
	movff	nowL, beforeL
	movff	nowH, beforeH	; Update "before" registers to prepare for next measurement loop
	return
	
diff_positive
	movlw	0x64		; 0x64 = 100 mV with our ADC settings
	subwf	differenceL
	movlw	0x00
	subwfb	differenceH	; (difference) - 100 mV
	btfss	STATUS, N
	call	towards_LDR2	; If (difference - 100mV) > 0 then servo will move one "step" & (before) registers are updated
				; If (difference - 100mV) < 0 then no movement of servo
	return
	
towards_LDR2
	movlw	0x01
	subwf	dc		; DECREMENT (?) duty cycle length by 1
	movff	nowL, beforeL
	movff	nowH, beforeH	; Update "before" registers to prepare for next measurement loop
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
	return
	
	
	; ******** ADC Settings ***************************************
ADC_Setup
	bsf	TRISA,RA0   ; use pin A0(==AN0) for input
	bsf	ANCON0,ANSEL0   ; set A0 to analog
	movlw   0x01	    ; select AN0 for measurement
	movwf   ADCON0	    ; and turn ADC on
	movlw   0x30	    ; Select 4.096V positive reference
	movwf   ADCON1	    ; 0V for -ve reference and -ve input
	movlw   0xF6	    ; Right justified output
	movwf   ADCON2	    ; Fosc/64 clock and acquisition times
	
	movlw	0x09
	movwf	beforeH	    
	movlw	0xC4		
	movwf	beforeL		; Initialise previous measurement of voltage to be 2.5 V = 2500 mV = 0x9C4
	return

ADC_Read
	bsf	ADCON0,GO	    ; Start conversion
adc_loop
	btfsc   ADCON0,GO	    ; check to see if finished
	bra	adc_loop
	return
	
	
	end