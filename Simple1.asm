		#include p18f87k22.inc
	
acs0	udata_acs
beforeH	    res 1   ; Reserve 8 bits for storage of D<11:8> from previous measurement
beforeL	    res	1   ; Reserve 8 bits for storage of D<7:0> from previous measurement
nowH	    res	1   ; Reserve 8 bits for storage of D<11:8> from current measurement	    
nowL	    res	1   ; Reserve 8 bits for storage of D<7:0> from current measurement
differenceH res	1   ; Temporary storage of difference between now and before voltages
differenceL res	1   ; Temporary storage of difference between now and before voltages
 
dc	    res	1   ; Software PWM 8-bit duty cycle length
delay_counter	res 1 ; Counter register for test delay
	    
	code
	org 0x0
	goto	setup
	
	; ******* Programme FLASH read Setup Code ***********************
setup	bcf	EECON1, CFGS	; point to Flash program memory  
	bsf	EECON1, EEPGD 	; access Flash program memory
	call	ADC_Setup	; setup ADC
	movlw	0x09
	movwf	beforeH	    
	movlw	0xC4		
	movwf	beforeL		; Initialise previous measurement of voltage to be 2.5 V = 2500 mV = 0x9C4
				; (When we put all of the code together, don't forget to set up PWM dc & period here!)
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
	
	; Depending on the sign of (difference) 
	btfsc	STATUS, N	
	call	diff_negative	; If (difference) is negative, execute this line
	btfss	STATUS, N	
	call	diff_positive	; If (difference) is positive, execute this line
	
	; Test that code is working
	movlw	0x00
	movwf	TRISC, ACCESS	; PORT C all outputs
	movff	dc, PORTC	; Output duty cycle length on PORT C
	call	test_delay	; Delay for x s so we can see duty cycle LEDs on PORT C
	
	; Stay in measurement loop
	goto	measure_loop		; goto current line in code

	
	
test_delay  ; For testing purposes only
	movlw	0xFF
	movwf	delay_counter
loop	decfsz	delay_counter
	bra	loop
	return
	
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
	return

ADC_Read
	bsf	    ADCON0,GO	    ; Start conversion
adc_loop
	btfsc   ADCON0,GO	    ; check to see if finished
	bra	    adc_loop
	return
	
	
	end