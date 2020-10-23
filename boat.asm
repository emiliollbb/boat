; vim: ft=asm_ca65

.segment "CHARS"
    .incbin "tiles.chr" ; if you have one
.segment "HEADER"
    .byte "NES",26,2,1 ; 32K PRG, 8K CHR
.segment "VECTORS"
    .word nmi, reset, 0
.segment "RODATA"
palette:
    .byte $0F,$31,$2A,$03,  $0F,$02,$31,$31,  $0F,$31,$31,$31,  $0F,$31,$31,$31  ;background palette
    .byte $0F,$2D,$3D,$01,  $0F,$28,$14,$21,  $0F,$39,$3A,$3B,  $0F,$3D,$3E,$3F  ;sprite palette data
end_palette:
s_palette = (end_palette - palette)
rosprites:
	     ;vert tile attr horiz
    .byte $50, $01, $00, $80   ;sprite 0
    .byte $50, $02, $00, $88   ;sprite 1
    .byte $50, $03, $00, $90   ;sprite 2
    .byte $58, $11, $00, $80   ;sprite 3
    .byte $58, $12, $00, $88   ;sprite 4
    .byte $58, $13, $00, $90   ;sprite 5
end_rosprites:
s_rosprites = (end_rosprites - rosprites)
background:
sky = $FD
water= $FD

    .res $20, sky  ;row 1, all sky
    .res $20, sky  ;row 2, all sky
    .res $20, sky  ;row 3, all sky
    .res $20, sky  ;row 4, all sky
    .res $20, sky  ;row 5, all sky
    .res $20, sky  ;row 6, all sky
    .res $20, sky  ;row 7, all sky
    .res $20, sky  ;row 8, all sky
    .res $20, sky  ;row 9, all sky
    .res $20, sky  ;row 10, all sky
    
	; row 11
    .byte sky,sky,sky,sky,sky,sky,sky,sky
    .byte sky,sky,sky,sky,sky,sky,sky,sky
    .byte sky,sky,sky,sky,sky,sky,sky,sky
    .byte sky,sky,sky,sky,sky,sky,sky,sky
    
    ; row 12
    .byte sky,sky,sky,sky,sky,sky,sky,sky
    .byte sky,sky,sky,sky,sky,sky,sky,sky
    .byte sky,sky,sky,sky,sky,sky,sky,sky
    .byte sky,sky,sky,sky,sky,sky,sky,sky
    
    .res $20, sky  ;row 13, all sky
    .res $20, sky  ;row 14, all sky
    .res $20, sky  ;row 15, all sky
    .res $20, sky  ;row 16, all sky
    .res $20, sky  ;row 17, all sky
    .res $20, sky  ;row 18, all sky
    .res $20, sky  ;row 19, all sky
    .res $20, sky  ;row 20, all sky
    .res $20, sky  ;row 21, all sky
    .res $20, sky  ;row 22, all sky
    .res $20, sky  ;row 23, all sky
    .res $20, sky  ;row 24, all sky
    .res $20, sky  ;row 25, all sky
    .res $20, sky  ;row 26, all sky
    .res $20, sky  ;row 27, all sky
    .res $20, sky  ;row 28, all sky
    .res $20, sky  ;row 29, all sky
    .res $20, sky  ;row 30, all sky
        

end_background:
s_background = (end_background - background)
attribute:
    .byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000 ; 2 tile rows
    .byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
    .byte %01010101, %01010101, %01010101, %01010101, %01010101, %01010101, %01010101, %01010101
    .byte %01010101, %01010101, %01010101, %01010101, %01010101, %01010101, %01010101, %01010101
    .byte %01010101, %01010101, %01010101, %01010101, %01010101, %01010101, %01010101, %01010101
    .byte %01010101, %01010101, %01010101, %01010101, %01010101, %01010101, %01010101, %01010101
    .byte %01010101, %01010101, %01010101, %01010101, %01010101, %01010101, %01010101, %01010101
    
end_attribute:
s_attribute = (end_attribute - attribute)
.segment "OAM"
sprites:
    .res s_rosprites
.segment "ZEROPAGE"
playerpos:
    .res $02, $00
buttons:
    .res $01, $00
pointer:
    .res 2   ; pointer variables are declared in RAM

.segment "CODE"

BUTTON_A = %10000000
BUTTON_B = %01000000
BUTTON_SELECT = %00100000
BUTTON_START = %00010000
BUTTON_UP = %00001000
BUTTON_DOWN = %00000100
BUTTON_LEFT = %00000010
BUTTON_RIGHT = %00000001

vblankwait:
    bit $2002
    bpl vblankwait
    rts

ReadController:
    lda #$01 ;latch the controller
    sta $4016
    lda #$00
    sta $4016
    ldx #$08
@loop:
    lda $4016
    lsr A           ; bit0 -> Carry
    rol buttons     ; bit0 <- Carry
    dex
    bne @loop
    rts

reset:
    sei          ; disable irqs
    cld          ; disable decimal mode
    ldx #$40
    stx $4017    ; disable apu frame irq
    ldx #$ff
    txs          ; set up stack
    inx          ; now x = 0
    stx $2000    ; disable nmi
    stx $2001    ; disable rendering
    stx $4010    ; disable dmc irqs

    jsr vblankwait; first wait for vblank to make sure ppu is ready

clrmem:
    lda #$00
    sta $0000, x
    sta $0100, x
    sta $0200, x
    sta $0300, x
    sta $0400, x
    sta $0500, x
    sta $0600, x
    sta $0700, x
    inx
    bne clrmem
   
    jsr vblankwait      ; second wait for vblank, ppu is ready after this

loadpalettes:
    lda $2002             ; read ppu status to reset the high/low latch
    lda #$3f
    sta $2006             ; write the high byte of $3f00 address
    lda #$00
    sta $2006             ; write the low byte of $3f00 address
    ldx #$00              ; start out at 0
loadpalettesloop:
    lda palette, x        ; load data from address (palette + the value in x)
                          ; 1st time through loop it will load palette+0
                          ; 2nd time through loop it will load palette+1
                          ; 3rd time through loop it will load palette+2
                          ; etc
    sta $2007             ; write to ppu
    inx                   ; x = x + 1
    cpx #s_palette              ; compare x to hex $10, decimal 16 - copying 16 bytes = 4 sprites
    bne loadpalettesloop  ; branch to loadpalettesloop if compare was not equal to zero
                        ; if compare was equal to 32, keep going down



;loadsprites:
;    ldx #$00              ; start at 0
;loadspritesloop:
;    lda rosprites, x        ; load data from address (rosprites +  x)
;    sta sprites, x          ; store into ram address (sprites + x)
;    inx                   ; x = x + 1
;    cpx #s_rosprites              ; compare x to hex $10, decimal 16
;    bne loadspritesloop   ; branch to loadspritesloop if compare was not equal to zero
                        ; if compare was equal to 16, keep going down

loadsprites:
    ldx #$00              ; start at 0
@loop:
    lda rosprites, x        ; load data from address (rosprites +  x)
    sta sprites, x          ; store into ram address (sprites + x)
    inx                   ; x = x + 1
    cpx #s_rosprites              ; compare x to hex $10, decimal 16
    bne @loop   ; branch to loadspritesloop if compare was not equal to zero

; Load empty sprites
;	     ;vert tile attr horiz
;    .byte $80, $00, $00, $80   ;sprite 0
loademptysprites:
@loop:
    lda #$F0        ; load data from address (rosprites +  x)
    sta sprites, x          ; store into ram address (sprites + x)
    inx                   ; x = x + 1
    lda #$00
    sta sprites, x
    inx
    sta sprites, x
    inx
    lda #$80
    sta sprites, x
    inx

    cpx #$F0              ; compare x to hex $FF, decimal 128
    bne @loop   ; branch to loadspritesloop if compare was not equal to zero
; end load empty sprites

                        
    lda #$80 ;set up player position
    sta playerpos
    sta playerpos+1
              
LoadBackground:
  LDA $2002             ; read PPU status to reset the high/low latch
  LDA #$20
  STA $2006             ; write the high byte of $2000 address
  LDA #$00
  STA $2006             ; write the low byte of $2000 address

  LDA #<background
  STA pointer       ; put the low byte of the address of background into pointer
  LDA #>background
  STA pointer+1       ; put the high byte of the address into pointer
  
  LDX #$00            ; start at pointer + 0
  LDY #$00
OutsideLoop:
  
InsideLoop:
    LDA (pointer), y  ; copy one background byte from address in pointer plus Y
  STA $2007           ; this runs 256 * 4 times
  
  INY                 ; inside loop counter
  CPY #$00
  BNE InsideLoop      ; run the inside loop 256 times before continuing down
  
  INC pointer+1       ; low byte went 0 to 256, so high byte needs to be changed now
  
  INX
  CPX #$04
  BNE OutsideLoop     ; run the outside loop 256 times before continuing down
              
              
loadattribute:
    lda $2002             ; read ppu status to reset the high/low latch
    lda #$23
    sta $2006             ; write the high byte of $23c0 address
    lda #$c0
    sta $2006             ; write the low byte of $23c0 address
    ldx #$00              ; start out at 0
loadattributeloop:
    lda attribute, x      ; load data from address (attribute + the value in x)
    sta $2007             ; write to ppu
    inx                   ; x = x + 1
    cpx #s_attribute              ; compare x to hex $08, decimal 8 - copying 8 bytes
    bne loadattributeloop  ; branch to loadattributeloop if compare was not equal to zero
                        ; if compare was equal to 128, keep going down

    lda #%10010000   ; enable nmi, sprites from pattern table 0, background from pattern table 1
    sta $2000

    lda #%00011110   ; enable sprites, enable background, no clipping on left side
    sta $2001

forever:
    jmp forever     ;jump back to forever, infinite loop
  
nmi:
    lda #<sprites
    sta $2003       ; set the low byte (00) of the ram address
    lda #>sprites
    sta $4014       ; set the high byte (02) of the ram address, start the transfer

    ; Read controller input and update positions
    jsr ReadController

    lda buttons
    and #BUTTON_UP
    beq :+
    dec playerpos+1
:
    lda buttons
    and #BUTTON_DOWN
    beq :+
    inc playerpos+1
:
    lda buttons
    and #BUTTON_LEFT
    beq :+
    dec playerpos
:
    lda buttons
    and #BUTTON_RIGHT
    beq :+
    inc playerpos
:
    ; We update the sprite positions here
    ;lda playerpos+1 ;vertical first
    ; Sprites 0,1,2
    ;sta sprites
    ;sta sprites+4
    ;sta sprites+8
    ;clc
    ; Sprites 3,4,5
    ;adc #$08
    ;sta sprites+12

    ; Sprites 0 and 2
    ;lda playerpos ;horizontal
    ;sta sprites+3
    ;sta sprites+19
    ;clc
    ; Sprites 1 and 3 (shifted right)
    ;adc #$08
    ;sta sprites+7
    ;sta sprites+15


    ;;this is the ppu clean up section, so rendering the next frame starts properly.
    lda #%10010000   ; enable nmi, sprites from pattern table 0, background from pattern table 1
    sta $2000
    lda #%00011110   ; enable sprites, enable background, no clipping on left side
    sta $2001
    
    lda #$00        ;;tell the ppu there is no background scrolling
    sta $2005
    sta $2005
  
    rti             ; return from interrupt
