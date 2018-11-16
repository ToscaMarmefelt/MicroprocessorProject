	#include p18f87k22.inc
	
	code
	org 0x0
	goto	setup
	
	; ******* Programme FLASH read Setup Code ***********************
setup	bcf	EECON1, CFGS	; point to Flash program memory  
	bsf	EECON1, EEPGD 	; access Flash program memory
	;call	UART_Setup	; setup UART
	;call	LCD_Setup	; setup LCD
	call	ADC_Setup	; setup ADC
	goto	measure_loop
	
measure_loop
	call	ADC_Read
	;movf	ADRESH,W
	;call	LCD_Write_Hex
	;movf	ADRESL,W
	;call	LCD_Write_Hex
	goto	measure_loop		; goto current line in code

	
ADC_Setup
	bsf	    TRISA,RA0	    ; use pin A0(==AN0) for input
	bsf	    ANCON0,ANSEL0   ; set A0 to analog
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
