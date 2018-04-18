  .inesprg 1   ; 1x 16KB PRG code
  .ineschr 1   ; 1x  8KB CHR data
  .inesmap 0   ; mapper 0 = NROM, no bank swapping
  .inesmir 1   ; background mirroring
  

;;;;;;;;;;;;;;;
	.rsset $0000
pointerLo .rs 1         ; Hi/Lo bytes for background data pointers
pointerHi .rs 1
counterLo .rs 1         ; Hi/Lo bytes for 16 bit counter
counterHi .rs 1
    
  .bank 0
  .org $C000 
RESET:
  SEI          ; disable IRQs
  CLD          ; disable decimal mode
  LDX #$40
  STX $4017    ; disable APU frame IRQ
  LDX #$FF
  TXS          ; Set up stack
  INX          ; now X = 0
  STX $2000    ; disable NMI
  STX $2001    ; disable rendering
  STX $4010    ; disable DMC IRQs


vblankwait1:       ; First wait for vblank to make sure PPU is ready
  BIT $2002
  BPL vblankwait1

clrmem:
  LDA #$00
  STA $0000, x
  STA $0100, x
  STA $0300, x
  STA $0400, x
  STA $0500, x
  STA $0600, x
  STA $0700, x
  LDA #$FE
  STA $0200, x
  INX
  BNE clrmem
   
vblankwait2:      ; Second wait for vblank, PPU is ready after this
  BIT $2002
  BPL vblankwait2


LoadPalettes:
  LDA $2002             ; read PPU status to reset the high/low latch
  LDA #$3F
  STA $2006             ; write the high byte of $3F00 address
  LDA #$00
  STA $2006             ; write the low byte of $3F00 address
  LDX #$00              ; start out at 0
LoadPalettesLoop:
  LDA palette, x        ; load data from address (palette + the value in x)
                          ; 1st time through loop it will load palette+0
                          ; 2nd time through loop it will load palette+1
                          ; 3rd time through loop it will load palette+2
                          ; etc
  STA $2007             ; write to PPU
  INX                   ; X = X + 1
  CPX #$20              ; Compare X to hex $10, decimal 16 - copying 16 bytes = 4 sprites
  BNE LoadPalettesLoop  ; Branch to LoadPalettesLoop if compare was Not Equal to zero
                        ; if compare was equal to 32, keep going down



LoadSprites:
  LDX #$00              ; start at 0
LoadSpritesLoop:
  LDA sprites, x        ; load data from address (sprites +  x)
  STA $0200, x          ; store into RAM address ($0200 + x)
  INX                   ; X = X + 1
  CPX #$10              ; Compare X to hex $10, decimal 16
  BNE LoadSpritesLoop   ; Branch to LoadSpritesLoop if compare was Not Equal to zero
                        ; if compare was equal to 16, keep going down
              
              
              
LoadBackground:
 LDA #LOW(background)
 STA pointerLo
 LDA #HIGH(background)
 STA pointerHi


  LDA $2002             ; read PPU status to reset the high/low latch
  LDA #$20
  STA $2006             ; write the high byte of $2000 address
  LDA #$00
  STA $2006             ; write the low byte of $2000 address
  LDY #$00              ; start out at 0
	LDX #$04              ; start out at 0
LoadBackgroundLoop:
	LDA [pointerLo], y
  STA $2007             ; write to PPU
  INY
	BNE LoadBackgroundLoop; X = X + 1
	INC pointerHi
	DEX
	BNE LoadBackgroundLoop; X = X + 1

              
              
LoadAttribute:
	;LDA #HIGH(attriutes)
	;STA pointerLo
	
  LDA $2002             ; read PPU status to reset the high/low latch
  LDA #$23
  STA $2006             ; write the high byte of $23C0 address
  LDA #$C0
  STA $2006             ; write the low byte of $23C0 address
  LDX #$00              ; start out at 0
LoadAttributeLoop:
  LDA attributes, x      ; load data from address
  STA $2007             ; write to PPU
  INX; X = X + 1
	;BNE LoadAttributeLoop
  CPX #$1E              ; Compare X to hex $08, decimal 8 - copying 8 bytes
  BNE LoadAttributeLoop  ; Branch to LoadAttributeLoop if compare was Not Equal to zero
                        ; if compare was equal to 128, keep going down


              
              
              
  LDA #%10010000   ; enable NMI, sprites from Pattern Table 0, background from Pattern Table 1
  STA $2000

  LDA #%00011110   ; enable sprites, enable background, no clipping on left side
  STA $2001
	
	;enable square wave sound
	lda #%00000001
  sta $4015 ;enable square 1
	
	

Forever:
  JMP Forever     ;jump back to Forever, infinite loop
  
 

NMI:
  LDA #$00
  STA $2003       ; set the low byte (00) of the RAM address
  LDA #$02
  STA $4014       ; set the high byte (02) of the RAM address, start the transfer


LatchController:
  LDA #$01
  STA $4016
  LDA #$00
  STA $4016       ; tell both the controllers to latch buttons


ReadA: 
  LDA $4016       ; player 1 - A
  AND #%00000001  ; only look at bit 0
  BEQ ReadADone   ; branch to ReadADone if button is NOT pressed (0)
                  ; add instructions here to do something when button IS pressed (1)
  LDA $0203       ; load sprite X position
  CLC             ; make sure the carry flag is clear
  ADC #$01        ; A = A + 1
  STA $0203       ; save sprite X position
	

 
  ;lda #%10111111 ;Duty 10, Volume F
  ;sta $4000
 
  ;lda #$C9    ;0C9 is a C# in NTSC mode
  ;sta $4002
  ;lda #$00
  ;sta $4003
	
	
	;lda #$00
	;sta $4002
	;lda #$00
	;sta $4003
	
ReadADone:        ; handling this button is done
  

ReadB: 
  LDA $4016       ; player 1 - B
  AND #%00000001  ; only look at bit 0
  BEQ ReadBDone   ; branch to ReadBDone if button is NOT pressed (0)
                  ; add instructions here to do something when button IS pressed (1)
  LDA $0203       ; load sprite X position
  SEC             ; make sure carry flag is set
  SBC #$01        ; A = A - 1
  STA $0203       ; save sprite X position
	
	lda #$00
	sta $4002
	lda #$00
	sta $4003
ReadBDone:        ; handling this button is done


  ;;This is the PPU clean up section, so rendering the next frame starts properly.
  LDA #%10010000   ; enable NMI, sprites from Pattern Table 0, background from Pattern Table 1
  STA $2000
  LDA #%00011110   ; enable sprites, enable background, no clipping on left side
  STA $2001
  LDA #$00        ;;tell the ppu there is no background scrolling
  STA $2005
  STA $2005
  
  RTI             ; return from interrupt
 
;;;;;;;;;;;;;;  
  
  
  
  .bank 1
  .org $E000
palette:
  .db $22,$29,$1A,$0F,  $22,$36,$17,$0F,  $22,$30,$21,$0F,  $22,$27,$17,$0F   ;;background palette
  .db $22,$1C,$15,$14,  $22,$02,$38,$3C,  $22,$1C,$15,$14,  $22,$02,$38,$3C   ;;sprite palette

sprites:
     ;vert tile attr horiz
  .db $80, $32, $00, $80   ;sprite 0
  .db $80, $33, $00, $88   ;sprite 1
  .db $88, $34, $00, $80   ;sprite 2
  .db $88, $35, $00, $88   ;sprite 3


background:
	.db $00,$01,$02,$03,$04,$05,$06,$07,$08,$09,$00,$01,$02,$03,$04,$05  ;;row 1
  .db $06,$07,$08,$09,$00,$01,$02,$03,$04,$05,$06,$07,$08,$09,$00,$01  ;;all sky
	.db $01,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 1
  .db $06,$07,$08,$09,$00,$01,$02,$03,$04,$05,$06,$07,$08,$09,$00,$01  ;;all sky
  .db $02,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 1
  .db $06,$07,$08,$09,$00,$01,$02,$03,$04,$05,$06,$07,$08,$09,$00,$01  ;;all sky
  .db $03,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 2
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;all sky
  .db $04,$24,$53,$54,$24,$24,$24,$24,$24,$24,$45,$45,$53,$54,$45,$45  ;;row 3
  .db $53,$54,$45,$45,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;brick tops
  .db $05,$24,$55,$56,$24,$24,$24,$24,$24,$24,$47,$47,$55,$56,$47,$47  ;;row 4
  .db $55,$56,$47,$47,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;brick bottoms
	.db $06,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 5
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;all sky
	.db $07,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 5
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;all sky
	.db $08,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 5
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$60,$61,$62,$63,$24,$24  ;;all sky	
  .db $09,$24,$24,$24,$31,$32,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 1
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$64,$65,$66,$67,$24,$24  ;;all sky
	.db $00,$24,$24,$30,$26,$34,$33,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 6
  .db $24,$24,$36,$37,$24,$24,$24,$24,$24,$24,$68,$69,$26,$6A,$24,$24  ;;all sky
	.db $38,$24,$30,$26,$26,$26,$26,$33,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 7
  .db $24,$35,$25,$25,$38,$24,$24,$24,$24,$24,$68,$69,$26,$6A,$24,$24  ;;all sky
	.db $B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5  ;;floor
	.db $B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5
	.db $B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7  ;;floor bottom
	.db $B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7
	.db $26,$B5,$26,$B5,$26,$B5,$26,$B5,$26,$B5,$26,$B5,$26,$B5,$26,$B5
	.db $26,$B5,$26,$B5,$26,$B5,$26,$B5,$26,$B5,$26,$B5,$26,$B5,$26,$B5
	.db $00,$01,$02,$03,$04,$05,$06,$07,$08,$09,$00,$01,$02,$03,$04,$05  ;;row 1
  .db $06,$07,$08,$09,$00,$01,$02,$03,$04,$05,$06,$07,$08,$09,$00,$01  ;;all sky
	.db $01,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 1
  .db $06,$07,$08,$09,$00,$01,$02,$03,$04,$05,$06,$07,$08,$09,$00,$01  ;;all sky
  .db $02,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 1
  .db $06,$07,$08,$09,$00,$01,$02,$03,$04,$05,$06,$07,$08,$09,$00,$01  ;;all sky
  .db $03,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 2
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;all sky
  .db $04,$24,$53,$54,$24,$24,$24,$24,$24,$24,$45,$45,$53,$54,$45,$45  ;;row 3
  .db $53,$54,$45,$45,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;brick tops
  .db $05,$24,$55,$56,$24,$24,$24,$24,$24,$24,$47,$47,$55,$56,$47,$47  ;;row 4
  .db $55,$56,$47,$47,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;brick bottoms
	.db $06,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 5
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;all sky
	.db $07,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 5
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;all sky
	.db $08,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 5
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$60,$61,$62,$63,$24,$24  ;;all sky	
  .db $09,$24,$24,$24,$31,$32,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 1
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$64,$65,$66,$67,$24,$24  ;;all sky
	.db $00,$24,$24,$30,$26,$34,$33,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 6
  .db $24,$24,$36,$37,$24,$24,$24,$24,$24,$24,$68,$69,$26,$6A,$24,$24  ;;all sky
	.db $38,$24,$30,$26,$26,$26,$26,$33,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 7
  .db $24,$35,$25,$25,$38,$24,$24,$24,$24,$24,$68,$69,$26,$6A,$24,$24  ;;all sky
	.db $B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5  ;;floor
	.db $B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5
	.db $B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7  ;;floor bottom
	.db $B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7
	.db $26,$B5,$26,$B5,$26,$B5,$26,$B5,$26,$B5,$26,$B5,$26,$B5,$26,$B5
	.db $26,$B5,$26,$B5,$26,$B5,$26,$B5,$26,$B5,$26,$B5,$26,$B5,$26,$B5
 
;attributes:
; .db $03,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
; .db $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
; .db $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
; .db $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$FF
attributes:
	.db $00,$00
	.db	$00,$00
	.db	$00,$00
	.db	$00,$00
	.db $0C,$00
	.db $0F,$0F
	.db $0F,$00
	.db $00,$00
	.db $00,$00
	.db $00,$00
	.db $00,$00
	.db $00,$00
	.db $0F,$00
	.db $0F,$00
	.db $00,$00

	



  .org $FFFA     ;first of the three vectors starts here
  .dw NMI        ;when an NMI happens (once per frame if enabled) the 
                   ;processor will jump to the label NMI:
  .dw RESET      ;when the processor first turns on or is reset, it will jump
                   ;to the label RESET:
  .dw 0          ;external interrupt IRQ is not used in this tutorial
  
  
;;;;;;;;;;;;;;  
  
  
  .bank 2
  .org $0000
  .incbin "mario.chr"   ;includes 8KB graphics file from SMB1
