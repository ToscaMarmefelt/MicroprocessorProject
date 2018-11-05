	#include p18f87k22.inc
	
	code
	org 0x0
	
rst	code	0x0000		    ; reset vector
	goto	start
	
	; ******* Set up high-priority interrupt vector
int_hi	code	0x0008		    ; high vector, no low vector
	btfss	INTCON,TMR0IF	    ; check that this is timer0 interrupt
	retfie	FAST		    ; if not then return
	incf	LATD		    ; increment PORTD
	bcf	INTCON,TMR0IF	    ; clear interrupt flag
	retfie	FAST		    ; fast return from interrupt
	
	; ******* Main program
main	code
	
start	clrf	TRISD		    ; Set PORTD as all outputs
	clrf	LATD		    ; Clear PORTD outputs
	movlw	b'10000111'	    ; Set timer0 to 16-bit, Fosc/4/256
	movwf	T0CON		    ; = 62.5KHz clock rate, approx 1sec rollover
	bsf	INTCON,TMR0IE	    ; Enable timer0 interrupt
	bsf	INTCON,GIE	    ; Enable all interrupts
	goto	$		    ; Sit in infinite loop

	
	end
