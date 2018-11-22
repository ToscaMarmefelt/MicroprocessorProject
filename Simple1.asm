		#include p18f87k22.inc
	
acs0	udata_acs
upper_limit_u	res	    1	    ; Upper limit of allowed voltage span <11:8>
upper_limit_l	res	    1	    ; Upper limit of allowed voltage span <7:0>
lower_limit_u	res	    1	    ; Lower limit of allowed voltage span <11:8>
lower_limit_l	res	    1	    ; Lower limit of allowed voltage span <7:0>
	
period_l	res	    1	    ; Lower byte of PWM period
period_u	res	    1	    ; Upper byte of PWM period
counter_l	res	    1	    ; Lower byte of PWM counter
counter_u	res	    1	    ; Upper byte of PWM counter
dc_pan		res	    1	    ; Software PWM 8-bit duty cycle length (HORIZONTAL rotation)
dc_tilt		res	    1	    ; Software PWM 8-bit duty cycle length (VERTICAL rotation)

temp		res	    1	    ; Temporary 8-bit storage 		
ADC_counter	res	    1	    ; 8-bit counter to decide time between servo updates
	    
	code
rst	org 0x0
	
	goto	setup
	
	
	; ******* Set up Timer0 Interrupt ***********************************************
init_hi	code	0x0008		    ; High-priority interrupt vector
	btfss	INTCON, TMR0IF	    ; If Timer0 has overflowed, then skip the next instruction line
	retfie	FAST		    ; If Timer0 has not overflowed yet, then return from interrupt
	
	incf	counter_l, F	    ; Increment PWM counter. Might give carry
	movlw	0x00
	addwfc	counter_u, F	    ; If carry from increment of counter_l, then add carry to counter_u
	
	; *** Compare software PWM counter to DC length and determine if output ofrespective signal should be high or low ***
	movf	dc_pan, W	    
	subwf	counter_l, W	    ; (counter_l - dc_pan) -> W . Potentially a borrow
	movlw	0x00		    
	subwfb	counter_u, W	    ; (counter_u - borrow) -> W 
	btfss   STATUS,N			    
	call	output_high_pan	    ; If counter < dc_pan : Output high on PORTD<0>
	btfsc	STATUS, N
	call	output_low_pan	    ; If counter > dc_pan : Output low on PORTD<0> 
	
	movf	dc_tilt, W
	subwf	counter_l, W	    ; (counter_l - dc_tilt) -> W . Potentially a borrow
	movlw	0x00		    
	subwfb	counter_u, W	    ; (counter_u - borrow) -> W 
	btfss   STATUS,N			    
	call	output_high_tilt    ; If counter < dc_tilt : Output high on PORTD<1>
	btfsc	STATUS, N
	call	output_low_tilt	    ; If counter > dc_tilt : Output low on PORTD<1> 
	
	; *** If we have reached end of PWM period, then reset counter ***
	movf	counter_l, W
	addlw	0x01		    ; (counter_l + 1) -> W. Potentially a carry
	subwfb	period_l, W	    ; (period_l) - (counter_l + 1) - (C) -> W. Potentially a borrow
	movf	counter_u, W
	subwfb	period_u, W	    ; (period_u) - (counter_u) - (C) -> W
	btfsc	STATUS, N	    ; Check sign of our calculation (period) - (counter + 1)
	call	reset_counter	    ; If (period) = (counter), then reset counter to zero
	
	; *** Exit Timer0 interrupt ***
	bcf	INTCON, TMR0IF	    ; Clear interrupt flag
	retfie	FAST		    ; Return from interrupt
	
		
	; ******* PWM Output calls ******* 
output_low_pan
	movlw	b'00000001'		    
	iorwf	PORTD, F	    ; Make PORTD<0>='1' and <7:1> remain the same
	return			    ; Output to PORTD
	
output_high_pan
	movlw	b'11111110'	
	andwf	PORTD, F	    ; Make PORTD<0>='0' and <7:1> remain the same
	return			    ; Output to PORTD
	
output_low_tilt
	movlw	b'00000010'	
	iorwf	PORTD, F	    ; Make PORTD<1>='1' and other pins remain unchanged
	return			    ; Output to PORTD
	
output_high_tilt
	movlw	b'11111101'	
	andwf	PORTD, F	    ; Make PORTD<1>='0' and other pins remain unchanged
	return			    ; Output to PORTD
	
reset_counter
	clrf	counter_l
	clrf	counter_u
	decf	ADC_counter, F	    ; Decrement counter for motor position update	
	return
	
	

	; ******* Set up Flash Program Memory and ADC & PWM functionalities  ************
setup	bcf	EECON1, CFGS	    ; Point to Flash program memory  
	bsf	EECON1, EEPGD	    ; Access Flash program memory
	call	ADC_Setup	    ; Setup ADC
	call	PWM_Setup	    ; Setup software PWM
	
	goto	main 
	
	
	
	; ******* MAIN PROGRAM ***********************************************************
main	call	ADC_Read_Pan	    ; Read analog input from pan voltage (AN0)
	call	move_servo	    ; Move towards LDR1 or LDR2 or remain still (HORIZONTAL ROTATION)
	call	ADC_Read_Tilt	    ; Read analog input from tilt voltage (AN1)
	call	move_servo	    ; Move towards LDR1 or LDR2 or remain still (VERTICAL ROATAION) 
	goto	main		    ; Stay in measurement loop
	
	
move_servo
	movf	upper_limit_l, W    ; (upper_limit_l) -> W
	subwf	ADRESL, W
	movwf	temp		    ; (ADRESL - upper_limit_l) -> (difference_l). Potentially a borrow
	movf	upper_limit_u, W    ; (upper_limit_u) -> W
	subwfb	ADRESH, W	    ; (ADRESH) - (upper_limit_u) - (borrow) -> W
	
	btfss	STATUS, N
	call	towards_LDR2	    ; If (ADC reading) > (upper limit): Move servo towards LDR2
	btfsc	STATUS, N
	call	check_lower_limit   ; If (ADC reading) < (upper limit): Compare against lower limit	

reading_delay			    ; *** Add delay to measurement loop (using reset_counter subroutine, called every 20 ms) ***
	tstfsz	ADC_counter	     
	bra	reading_delay	    ; While ADC_counter != 0, wait in loop
	movlw	0xA		    ; Reset ADC_counter to .10
	movwf	ADC_counter	    
	return	   
	
	
check_lower_limit
	movf	lower_limit_l, W    ; (lower_limit_l) -> W
	subwf	ADRESL, W
	movwf	temp		    ; (lower_limit_l - ADRESL) -> (difference_l). Potentially a borrow
	movf	lower_limit_u, W
	subwfb	ADRESH, W	    ; (ADRESH - lower_limit_u - borrow) -> W
	btfsc	STATUS, N
	call	towards_LDR1	    ; If (ADC reading) < (lower limit): Move servo towards LDR1
	return			    ; otherwise, remain in the same position

towards_LDR1
	btfss	ADCON0, 2	     
	movff	dc_pan, temp	    ; If ADCON0<2>='0', we are measuring pan voltage
	btfsc	ADCON0, 2	     
	movff	dc_tilt, temp	    ; If ADCON0<2>='1', we are measuring tilt voltage
	movlw	0x40		    ; Min duty cycle length is (min) = 1 ms = .63 counts = 0x3F counts. (min + 1) -> W
	subwf	temp, W		    ; (dc) - (min + 1)
	
	btfsc	STATUS, N	    ; If (dc) = (min), then let duty cycle length remain unchanged and return from call
	return			    ; If (dc) > (min), then decrement relevant dc length by 1
	btfss	ADCON0, 2	    ; ADCON0<2>='0': pan voltage
	decf	dc_pan		     
	btfsc	ADCON0, 2	    ; ADCON0<2>='1': tilt voltage
	decf	dc_tilt		 
	return			    
	
	
towards_LDR2
	btfss	ADCON0, 2	    
	movff	dc_pan, temp	    ; If ADCON0<2>='0', we are measuring pan voltage
	btfsc	ADCON0, 2
	movff	dc_tilt, temp	    ; If ADCON0<2>='1', we are measuring tilt voltage
	movlw	0x7C		    ; Max duty cycle length is (max) = 2 ms = .125 counts = 0x7D. (max - 1) -> W
	subwf	temp, W		    ; (dc) - (max - 1) -> W
	
	btfss	STATUS, N	    ; If (dc) = (max), then let duty cycle length remain unchanged and return from call
	return			    ; If (dc) < (max), then increment relevant dc length by 1
	btfss	ADCON0, 2	    ; ADCON0<2>='0': pan voltage
	incf	dc_pan		     
	btfsc	ADCON0, 2	    ; ADCON0<2>='1': tilt voltage
	incf	dc_tilt		     
	return			    


	
	; ******** PWM Settings ***************************************
PWM_Setup
	movlw	0x04
	movwf	period_u
	movlw	0xE2
	movwf	period_l	    ; Set PWM period to 0x04E2 = .1250 counts
	call	reset_counter	    ; Start PWM counter at 0
	movlw	0x5E		    ; 0x5E = .94		    
	movwf	dc_pan		    ; Initialise PWM duty cycles to 1.5 ms = 0 degrees = .94 counts
	movwf	dc_tilt		    
	
	; *** Output port settings ***
	movlw	0xFC		    ; = b'1111 1100' 
	movwf	TRISD, ACCESS	    ; Make PORT D <1:0> outputs
	clrf	LATD, ACCESS	    ; Clear PORT D outputs
	
	; *** Timer0 settings ***
	movlw	b'11001000'	    ; TMR0 on. 8-bit counter, Fcyc = Fosc/4/(prescaler value=1)
	movwf	T0CON, ACCESS	    ; Clockrate = 62.5 kHz
	
	bsf	INTCON, TMR0IE	    ; Enable Timer0 interrupt
	bsf	INTCON, GIE	    ; Enable all interrupts
	return		
	
	
	
	; ******** ADC Settings ***************************************
ADC_Setup
	bsf	TRISA, RA0	; Pin A0(==AN0) input of pan signal
	bsf	TRISA, RA1	; Pin A1(==AN1) input of tilt signal
	bsf	ANCON0, ANSEL0  ; Set A0 to analog
	bsf	ANCON0, ANSEL1	; Set A1 to analog
	movlw   0x01		; Select AN0 for first measurement
	movwf   ADCON0		; and turn ADC on
	movlw   0x30		; Select 4.096V positive reference
	movwf   ADCON1		; 0V for -ve reference and -ve input
	movlw   0xF6		; Right justified output
	movwf   ADCON2		; Fosc/64 clock and acquisition times
				
	movlw	0x0A		; *** Set allowed span of voltages for LDR equilibrium ***
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

ADC_Read_Pan
	movlw	0x03		; Select AN0 for measurement
	movwf	ADCON0		; and start conversion
adc_loop_pan
	btfsc   ADCON0,GO	; Wait in loop until measurement is finished
	bra	adc_loop_pan
	return
	
ADC_Read_Tilt
	movlw	0x07		; Select A1 for measurement
	movwf	ADCON0		; and start conversion
adc_loop_tilt
	btfsc	ADCON0, GO	; Wait in loop until measurement is finished
	bra	adc_loop_tilt
	return
	
	
	end