	#include p18f87k22.inc

	extern	UART_Setup, UART_Transmit_Message   ; external UART subroutines
	extern  LCD_Setup, LCD_Write_Message	    ; external LCD subroutines
	extern	LCD_Write_Hex			    ; external LCD subroutines
	extern  ADC_Setup, ADC_Read		    ; external ADC routines
	
acs0	udata_acs   ; reserve data space in access ram
counter	    res 1   ; reserve one byte for a counter variable
delay_count res 1   ; reserve one byte for counter in the delay routine

	; ******* Reserve data space for 8-bit by 16-bit multiplication
eight	res 1
sixteen_l   res 1
sixteen_u   res	1
temp0	res 1	    ; 1 byte for temporary use
res0	res 1	    ; 1 byte for lowest 8 bits of result
res1	res 1	    ; 1 byte for mid 8 bits of result
res2	res 1	    ; 1 byte for upper  8 bits of result
	
	; ******* Reserve data space for 16-bit by 16-bit multiplication
sixteen2_l  res 1   ; Assign values for second 16-bit number to multiply by
sixteen2_u  res 1
prod0	res 1	    ; In total product (prod0:3) will require 4 bytes (32 bits) 
prod1	res 1
prod2	res 1
prod3	res 1
	
	; ******* Reserve data space for 8-bit by 24-bit multiplication
l_24	res 1   ; 1 byte for each of the low, middle and upper bytes of 24-bit number
m_24	res 1
u_24	res 1
  
;tables	udata	0x400    ; reserve data anywhere in RAM (here at 0x400)
;myArray res 0x80    ; reserve 128 bytes for message data

rst	code	0    ; reset vector
	goto	setup

pdata	code    ; a section of programme memory for storing data
	; ******* myTable, data in programme memory, and its length *****
;myTable data	    "Hello World!\n"	; message, plus carriage return
;	constant    myTable_l=.13	; length of data
	
main	code
	; ******* Programme FLASH read Setup Code ***********************
setup	bcf	EECON1, CFGS	; point to Flash program memory  
	bsf	EECON1, EEPGD 	; access Flash program memory
	call	UART_Setup	; setup UART
	call	LCD_Setup	; setup LCD
	call	ADC_Setup	; setup ADC
	;goto	start
	
	
	; ******* Code to check 8x16-bit multiplication
;test816	movlw	0x1F		
;	movwf	eight		; Set value for 8-bit number
;	movlw	0xFF
;	movwf	sixteen_l	; Value for lower byte of 16-bit number
;	movlw	0x03
;	movwf	sixteen_u	; Value for upper byte of 16-bit number
	
;	call	mul8by16	; Multiply 8-bit number by 16-bit number
				; Result stored in res2:0
	
	; ******* Code to check 16x16-bit numtiplication
;test1616
	;movlw	0xF8		; First 16-bit number: 0xF81D
	;movwf	sixteen_u	; upper byte
	;movlw	0x1D
	;movwf	sixteen_l	; lower byte
	;movlw	0xBB		; Second 16-bit number: 0xBB92
	;movwf	sixteen2_u	; upper byte
	;movlw	0x92
	;movwf	sixteen2_l	; lower byte
	

	;call	mul16by16	; Multiply 16-bit number by 16-bit number
				; Result stored in prod3:0
	
	
	; ******* Code to check 8x24-bit multiplication
;test824	movlw	0x74		
;	movwf	eight		; Set value for 8-bit number
;	movlw	0x0A
;	movwf	l_24	; Value for lower byte of 24-bit number
;	movlw	0xF2
;	movwf	m_24	; Value for mid byte of 24-bit number	
;	movlw	0xEF
;	movwf	u_24		; Value for upper byte of 24-bit number
	
;	call	mul8by24
	
	
	; ******* ADC test code *******
	
	
				
	; 8-bit by 16-bit multiplication
mul8by16    
	movf	sixteen_l, W	; Move lower byte of 16-bit number to W
	mulwf	eight		; Multiply 8-bit number by lower byte of 16-bit number
	movff	PRODL, res0	; Lower byte of product at address res0
	movff	PRODH, temp0	; Upper byte of product in temporary storage temp0
	
	movf	sixteen_u, W	; Move upper byte of 16-bit number to W
	mulwf	eight		; Multiply 8-bit number by upper byte of 16-bit number
	
	movf	temp0, W	; Move upper byte of first product to W
	addwf	PRODL, W	; Add upper byte of first product to lower byte of last product. Will give a carry!
	movwf	res1		; Move result to address res1
	movlw	0x00
	movwf	res2
	movf	PRODH, W	; Move upper byte of second product to W	
	addwfc	res2, F		; Add W, res2 (=empty) and carry bit. Place in res2. 
	return
	
	; 8-bit by 24-bit multiplication
mul8by24
	movf	l_24, W		; Move lower byte to W
	mulwf	eight		; Multiply 8-bit number by lower byte of 24-bit number
	movff	PRODL, prod0	; Final result of lowest byte of product
	movff	PRODH, prod1	; Temporary storage
	
	
	movf	m_24, W		; Move mid byte to W
	mulwf	eight		; Multiply 8-bit number by mid byte of 24-bit number
	movff	PRODH, prod2	; Temporary storage
	movf	PRODL, W
	addwf	prod1, F	; Second byte of first multiplication + first byte of second multiplication. Might give carry!
	
	movf	u_24, W		; Move upper byte to W
	mulwf	eight		; Multiply 8-bit number by upper byte of 24-bit number
	movf	PRODL, W
	addwfc	prod2, F	; Second byte of second multiplication + first byte of third multiplication. Might give carry!
	movlw	0x00
	movwf	prod3		; Make sure prod3 is empty beforehand
	movf	PRODH, W
	addwfc	prod3		; Second byte of third multiplication + carry
	return
	
	
	; 16-bit by 16-bit multiplication
mul16by16   
	movff	sixteen2_l, eight  
	call	mul8by16	; Multiply lower byte of second number by first number
	movff	res0, prod0	; Final output of lower byte
	movff	res1, prod1	; Temporary storage
	movff	res2, prod2	; Temporary storage
	
	movff	sixteen2_u, eight
	call	mul8by16	; Multiply upper byte of second number by first number
	
	movf	res0, W		
	addwf	prod1, F	; Add lower byte of second multiplication to second byte of first multiplication. Might give carry!
	movf	res1, W
	addwfc	prod2, F	; Add upper byte of first multiplication to second byte of second multiplication and carry. Might give carry!
	movlw	0x00
	movwf	prod3		; Make sure prod3 is empty
	movf	res2, W
	addwfc	prod3, F
	return 
	
	
	
	; ******* Main programme ****************************************
;start 	;lfsr	FSR0, myArray	; Load FSR0 with address in RAM	
	;movlw	upper(myTable)	; address of data in PM
	;movwf	TBLPTRU		; load upper bits to TBLPTRU
	;movlw	high(myTable)	; address of data in PM
	;movwf	TBLPTRH		; load high byte to TBLPTRH
	;movlw	low(myTable)	; address of data in PM
	;movwf	TBLPTRL		; load low byte to TBLPTRL
	;movlw	myTable_l	; bytes to read
	;movwf 	counter		; our counter register
;loop 	;tblrd*+			; one byte from PM to TABLAT, increment TBLPRT
	;movff	TABLAT, POSTINC0; move data from TABLAT to (FSR0), inc FSR0	
	;decfsz	counter		; count down to zero
	;bra	loop		; keep going until finished
		
	;movlw	myTable_l-1	; output message to LCD (leave out "\n")
	;lfsr	FSR2, myArray
	;call	LCD_Write_Message

	;movlw	myTable_l	; output message to UART
;	lfsr	FSR2, myArray
	;call	UART_Transmit_Message
	
measure_loop
	call	ADC_Read
	movf	ADRESH,W
	call	LCD_Write_Hex
	movf	ADRESL,W
	call	LCD_Write_Hex
	goto	measure_loop		; goto current line in code

	; a delay subroutine if you need one, times around loop in delay_count
;delay	;decfsz	delay_count	; decrement until zero
	;bra delay
	;return

	end