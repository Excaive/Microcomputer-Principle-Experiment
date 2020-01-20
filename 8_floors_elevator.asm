;386ex MASTER /SLAVE  8259A INT ,MASTER=MIR5,SLAVE=SIR0  MASTER INT TYPE=30H-37H;SLAVE INT TYPE =70H-77H
.model small
.386

;~~~~~~~~~~~���ݶ�~~~~~~~~~~~~~
data   segment
	i8255_a    equ  200h
	o8255_b    equ  201h
	o8255_c    equ  202h
	i8255_kz   equ  203h
      
	i8253_0    equ  210h
	i8253_1    equ  211h
	i8253_kz   equ  213h

	iled_d     equ	220h	;led����ڵ�ַ
	iled_w     equ	221h	;ledλ��ڵ�ַ

	djcode     db   01h,03h,02h,06h,04h,0ch,08h,09h	;��������
	djp        dw   0000h				;��������ָ�� bx
	loopn      dw	20	;������а�Ȧ��ѭ�������������

	direc      db   00h	;�������з��� 0��ͣ 1���� 2����
	floor      db   00h     ;��ǰ����¥��
	floor_up   db   00h,00h,00h,00h,00h,00h,00h,00h ;����ͣ��¥��
	floor_dn   db   00h,00h,00h,00h,00h,00h,00h,00h ;����ͣ��¥��
	next       db   00h	;��һͣ��¥��
	tflag      db	00h	;ת���־
	bj         db   00h	;�Ƿ񱨾� 0���� 1����

	keyin      db	00000000b	;����İ����ź�
	keynew     db	00000000b	;���ζ���İ����ź����ϴ������0��1��¥��
	keylist    db	00000001b,00000010b,00000100b,00001000b,00010000b,00100000b,01000000b,10000000b	;�����ź��б�

	delayt     dd	10000h	;�ӳ�ʱ��
	djdelayt   dd	10000h	;���������ת�ӳ�ʱ��

	ledfloor   db   06h,5bh,4fh,66h,6dh,7dh,07h,7fh,67h	;led¥������б�
	leddirec   db	00h,1ch,23h				;led��������б�
	lflag      db	00h	;led���������ʾ��־
	lcycle     db	10h	;led�������ѭ������

	rflag      db	00h	;��������Ƶ�ʱ�־ 0���� 4����������(3) 5����������(1) 6��������(6)
	rcycle     db	00h	;��������ѭ������
	rlevel     db	00000000b	;�������õ�ƽ��־
	
data   ends


;~~~~~~~~~~~������~~~~~~~~~~~~~~~   
code segment  
         assume cs:code,ds:data 
start:   cli
         mov   ax,data
         mov   ds,ax
         mov   es,ax  
;------- 8255 init---------------  
	 mov   dx,i8255_kz
         mov   al,90h       ;����8255������,A������,B�����,(C�����)
         out   dx,al
;------- 8254 init--------------- ÿ250us���һ�ΰ���   
         mov   dx,i8253_kz    
         mov   al,36h       ;��8254ͨ��0������ʽ2
         out   dx,al
         mov   ax,25        ;д��ѭ��������ֵ75
         mov   dx,i8253_0
         out   dx,al        ;��д����ֽ�
         mov   al,ah
         out   dx,al        ;��д����ֽ�
         mov   dx,i8253_kz
         mov   al,76h       ;��8254ͨ��1������ʽ2
         out   dx,al
         mov   ax,10        ;д��ѭ��������ֵ10
         mov   dx,i8253_1
         out   dx,al        ;��д���ֽ�
         mov   al,ah
         out   dx,al        ;��д���ֽ�
;------ 8259 init----------------      
         push  ds    
         mov   ax,0
         mov   ds,ax
         lea   ax,cs:mint_proc  ;WRITE INTERRUPT PROGRAM EMTRY ADDRESS
         mov   bx,5             ;n=IRx
         mov   si,30H           ;base =30H   Master
         add   si,bx
         add   si,si            ;type num
         add   si,si            ;X4
         mov   ds:[si],ax
         push  cs            
         pop   ax   
         mov   ds:[si+2],ax   
         pop   ds                 
;------ WRITE 8259 MASK WORD-----------
         in    al,21h    
         and   al,11011011b    ;mask byte       
         out   21h,al
         sti


	mov	bx,0
	mov	bl,floor
	call	shfloor
  
main_0:				;���ݾ�ֹ
	mov	direc,00h
	mov	tflag,00h
 m0_loop:
	call	tebj
	call	dobj
	mov	al,direc

	mov	ah,bj
	cmp	ah,1
	je	main_0

	cmp	al,1
	jg	main_up
	je	main_dn
	jmp	m0_loop		


main_up:			;��������
	mov	direc,02h
	mov	tflag,00h
 mu_new:		;����
	call	tenext
	mov	cl,next
	mov	bl,floor
	cmp	bl,cl
	je	mu_z

	call	spd_up
	jmp	mu_su
 mu_loop:		;x��
	mov	bl,floor
	inc	bl
	mov	floor,bl
	call	shfloor
	call	run
  mu_su:		;x.5��
	call	tenext
	mov	cl,next
	mov	dl,floor
	inc	dl
	cmp	dl,cl
	je	mu_sd
	call	run
	jmp	mu_loop

 mu_sd:			;����ͣ��
	call	spd_dn
	mov	bl,floor
	inc	bl
	mov	floor,bl
	call	shfloor
	mov	delayt,010000000h
	mov	rflag,04h
	call	delay
	mov	rflag,05h
	call	delay
	mov	rflag,00h
	call	delayn
  mu_bjlp:
	call	tebj
	call	dobj
	mov	ch,bj
	cmp	ch,1
	je	mu_bjlp
	jmp	mu_new		
	
 mu_z:			;�˳�����
	call	clearup
	mov	ah,tflag
	cmp	ah,1
	je	main_dn
	jmp	main_0


main_dn:			;��������
	mov	direc,01h
	mov	tflag,00h
 md_new:		;����
	call	tenext
	mov	cl,next
	mov	bl,floor
	cmp	bl,cl
	je	md_z

	call	spd_up
	jmp	md_su
 md_loop:		;x��
	mov	bl,floor
	dec	bl
	mov	floor,bl
	call	shfloor
	call	run
  md_su:		;x.5��
	call	tenext
	mov	cl,next
	mov	dl,floor
	dec	dl
	cmp	dl,cl
	je	md_sd
	call	run
	jmp	md_loop

 md_sd:			;����ͣ��
	call	spd_dn
	mov	bl,floor
	dec	bl
	mov	floor,bl
	call	shfloor
	mov	delayt,010000000h
	mov	rflag,04h
	call	delay
	mov	rflag,05h
	call	delay
	mov	rflag,00h
	call	delayn
  md_bjlp:
	call	tebj
	call	dobj
	mov	ch,bj
	cmp	ch,1
	je	md_bjlp
	jmp	md_new

 md_z:			;�˳�����
	call	cleardn
	mov	ah,tflag
	cmp	ah,1
	je	main_up
	jmp	main_0


;------Master INT Prog------------------
mint_proc proc far   
         sti
         push  ax
         call  scan
	 call  ledshow
	 call  ring   
         mov   al,20h  
         out   20h,al  
         pop   ax         
         iret
mint_proc  endp

  
scan	proc   
	push	ax
	push	bx
	push	cx
	push	dx
	push	si

	mov	dx,i8255_a
	in	al,dx
	mov	cl,keyin
	cmp	al,cl
	je	exitmint

	mov	keyin,al
	not	cl
	and	al,cl
	mov	keynew,al
	mov	al,direc
	mov	ah,floor
	mov	cl,00000001b
	mov	bx,0

	cmp	al,0
	jne	m_uod
m_0:			;���ݾ�ֹ
	mov	dl,keynew
	and	dl,cl	
	jz	exitm_0
	cmp	bl,ah	;floor
	jg	m_0_up
	jl	m_0_dn
	jmp	exitm_0
  m_0_up:		;�ı�����ͣ��¥��
	mov	si,offset floor_up
	mov	byte ptr[si+bx],01h
	mov	dl,02h
	mov	direc,dl
	jmp	exitmint
  m_0_dn:		;�ı�����ͣ��¥��
	mov	si,offset floor_dn
	mov	byte ptr[si+bx],01h
	mov	dl,01h
	mov	direc,dl	
	jmp	exitmint
 exitm_0:
	inc	bl
	add	cl,cl
	cmp	bl,8
	jne	m_0
	jmp	exitmint

m_uod:			;�������������½�
	cmp	al,2
	jne	m_d
m_u:			;����������
	mov	dl,keynew
	and	dl,cl
	jz	exitm_u
	cmp	bl,ah	;floor	
	jng	m_u_dn
  m_u_up:		;�ı�����ͣ��¥��
	mov	si,offset floor_up
	mov	byte ptr[si+bx],01h
	jmp	exitm_u	
  m_u_dn:		;�ı�����ͣ��¥��
	mov	si,offset floor_dn
	mov	byte ptr[si+bx],01h
	mov	tflag,01h	
	jmp	exitm_u
 exitm_u:
	inc	bl
	add	cl,cl
	cmp	bl,8
	jne	m_u
	jmp	exitmint

m_d:			;�������½�
	mov	dl,keynew
	and	dl,cl
	jz	exitm_d
	cmp	bl,ah 	;floor	
	jnl	m_d_up
  m_d_dn:		;�ı�����ͣ��¥��
	mov	si,offset floor_dn
	mov	byte ptr[si+bx],01h	
	jmp	exitm_d
  m_d_up:		;�ı�����ͣ��¥��	
	mov	si,offset floor_up
	mov	byte ptr[si+bx],01h
	mov	tflag,01h
	jmp	exitm_d
 exitm_d:
	inc	bl
	add	cl,cl
	cmp	bl,8
	jne	m_d
	jmp	exitmint	
exitmint:

	pop	si
	pop	dx
	pop	cx
	pop	bx
	pop	ax 
	ret    
scan	endp  


ledshow	proc
	push	ax
	push	bx
	push	cx
	push	dx
	push	si

	mov	bx,0
	mov	cl,lflag
	cmp	cl,0
	je	showd
showf:
	mov	si,offset ledfloor
	mov	bl,floor
	mov	al,[si+bx]
	mov	dx,iled_d
	out	dx,al
	mov	al,01h
	mov	dx,iled_w
	out	dx,al
	jmp	ldec
showd:
	mov	si,offset leddirec
	mov	bl,direc
	mov	al,[si+bx]
	mov	dx,iled_d
	out	dx,al
	mov	al,04h
	mov	dx,iled_w
	out	dx,al
	jmp	ldec

ldec:
	mov	ch,lcycle
	cmp	ch,0
	jne	exitlsh
	not	cl
	mov	ch,10h
exitlsh:
	dec	ch
	mov	lflag,cl
	mov	lcycle,ch

	pop	si
	pop	dx
	pop	cx
	pop	bx
	pop	ax 
	ret    
ledshow	endp


ring	proc   
	push	ax
	push	bx
	push	cx
	push	dx
	push	si

	mov	cl,rflag
	cmp	cl,0
	je	exitring

	mov	bl,rcycle
	cmp	bl,0
	jne	doring

	mov	bl,rflag
	mov	al,rlevel
	cmp	al,00000000b
	jne	rlv0
rlv1:
	mov	al,10000000b
	mov	rlevel,al
	jmp	doring
rlv0:
	mov	al,00000000b
	mov	rlevel,al
	jmp	doring	
doring:	
	dec	bl
	mov	dx,o8255_c
	out	dx,al
	mov	rcycle,bl

exitring:
	pop	si
	pop	dx
	pop	cx
	pop	bx
	pop	ax 
	ret    
ring	endp  


;~~~~~~~~~~~�ӳ���~~~~~~~~~~~~~~~ 
spd_up	proc near		;���������������а�Ȧ
	push	ax
	push	bx
	push	cx
	push	dx
	push	si

	mov	delayt,80000h
	mov	cx,loopn
	mov	dx,o8255_c
	mov	si,offset djcode
	mov	bx,djp	
	mov	al,direc
	cmp	al,1
	je	su_d_lp

su_u_lp:	;�������а�Ȧ
	mov     al,[si+bx]
	out	dx,al
	call	delay
	call	incp
	call	decdt		
	loop	su_u_lp

	jmp	exitsu

su_d_lp:	;�������а�Ȧ
	mov     al,[si+bx]
	out	dx,al
	call	delay
	call	decp
	call	decdt
	loop	su_d_lp

exitsu:
	mov	djp,bx
	mov	ecx,delayt
	mov	djdelayt,ecx

	pop	si
	pop	dx
	pop	cx
	pop	bx
	pop	ax
	ret
spd_up	endp


spd_dn	proc near		;����ͣ���������а�Ȧ
	push	ax
	push	bx
	push	cx
	push	dx
	push	si

	mov	eax,djdelayt
	mov	delayt,eax
	mov	cx,loopn
	mov	dx,o8255_c
	mov	si,offset djcode
	mov	bx,djp	
	mov	al,direc
	cmp	al,1
	je	sd_d_lp

sd_u_lp:	;�������а�Ȧ
	mov     al,[si+bx]
	out	dx,al
	call	incdt
	call	delay
	call	incp
	loop	sd_u_lp	

	jmp	exitsd

sd_d_lp:	;�������а�Ȧ
	mov     al,[si+bx]
	out	dx,al
	call	incdt
	call	delay
	call	decp
	loop	sd_d_lp

exitsd:
	mov	djp,bx

	pop	si
	pop	dx
	pop	cx
	pop	bx
	pop	ax
	ret
spd_dn	endp



run	proc near		;�����������а�Ȧ
	push	ax
	push	bx
	push	cx
	push	dx
	push	si

	mov	eax,djdelayt
	mov	delayt,eax
	mov	cx,loopn
	mov	dx,o8255_c
	mov	si,offset djcode
	mov	bx,djp	
	mov	al,direc
	cmp	al,1
	je	r_d_lp

r_u_lp:		;�������а�Ȧ
	mov     al,[si+bx]
	out	dx,al
	call	delay
	call	incp
	loop	r_u_lp

	jmp	exitr

r_d_lp:		;�������а�Ȧ
	mov     al,[si+bx]
	out	dx,al
	call	delay
	call	decp
	loop	r_d_lp

exitr:
	mov	djp,bx

	pop	si
	pop	dx
	pop	cx
	pop	bx
	pop	ax
	ret
run	endp


incp	proc near	;��������ָ��+1����λ������1/8Ȧ��
	inc	bx
	cmp	bx,8
	jne	exitincp
	mov	bx,0
exitincp:
	ret
incp	endp


decp	proc near	;��������ָ��-1����λ������1/8Ȧ��
	cmp	bx,0
	jne	exitdecp
	mov	bx,8
exitdecp:
	dec	bx
	ret
decp	endp


tenext	proc near	;������һͣ��¥��
	push	ax
	push	bx
	push	cx
	push	dx
	push	si

	mov	bx,0
	mov	al,direc	
	mov	bl,floor
	cmp	al,1
	jg	te_up
	je	te_dn
te_0:
	jmp	exitte	
te_up:
	mov	si,offset floor_up
	cmp	bl,7
	je	exitte
	inc	bl
	mov	cl,[si+bx]
	cmp	cl,0
	je	te_up
	mov	next,bl
	jmp	exitte
te_dn:
	mov	si,offset floor_dn
	cmp	bl,0
	je	exitte
	dec	bl
	mov	cl,[si+bx]
	cmp	cl,0
	je	te_dn
	mov	next,bl
	jmp	exitte

exitte:
	pop	si
	pop	dx
	pop	cx
	pop	bx
	pop	ax
	ret
tenext	endp


tebj	proc near	;���Ե�ǰ����¥��Ŀ��أ���������bj��1
	push	ax
	push	bx
	push	cx
	push	dx
	push	si

	mov	bx,0
	mov	bl,floor
	mov	cl,keyin
	mov	si,offset keylist
	mov	dl,[si+bx]
	and	dl,cl
	jnz	tebj1
tebj0:
	mov	al,00h
	mov	bj,al
	jmp	exitteb
tebj1:
	mov	al,01h
	mov	bj,al
	jmp	exitteb

exitteb:
	pop	si
	pop	dx
	pop	cx
	pop	bx
	pop	ax
	ret
tebj	endp


dobj	proc near	;��bj==1�򱨾�
	push	ax
	push	bx
	push	cx
	push	dx
	push	si

	mov	al,bj
	cmp	al,0
	je	exitdob

	mov	bx,0
	mov	bl,floor
	mov	si,offset keylist
	mov	cl,[si+bx]
	mov	delayt,10000000h
	mov	dx,o8255_b
	mov	al,00h
	out	dx,al
	mov	rflag,00h
	call	delay
	call	delay
	mov	al,cl
	out	dx,al
	mov	rflag,06h
	call	delay
	call	delay
	mov	rflag,00h	

exitdob:
	pop	si
	pop	dx
	pop	cx
	pop	bx
	pop	ax
	ret
dobj	endp


shfloor	proc near	;����ǰ����¥����led��ʾ����
	push	ax
	push	bx
	push	cx
	push	dx
	push	si

	mov	bx,0
	mov	bl,floor
	mov	si,offset keylist
	mov	al,[si+bx]
	mov	dx,o8255_b
	out	dx,al

	pop	si
	pop	dx
	pop	cx
	pop	bx
	pop	ax
	ret
shfloor	endp


clearup	proc near	;��floor_up����
	push	si

	mov	si,offset floor_up
	mov	byte ptr[si],00h
	mov	byte ptr[si+01h],00h
	mov	byte ptr[si+02h],00h
	mov	byte ptr[si+03h],00h
	mov	byte ptr[si+04h],00h
	mov	byte ptr[si+05h],00h
	mov	byte ptr[si+06h],00h
	mov	byte ptr[si+07h],00h

	pop	si
	ret
clearup	endp


cleardn	proc near	;��floor_dn����
	push	si

	mov	si,offset floor_dn
	mov	byte ptr[si],00h
	mov	byte ptr[si+01h],00h
	mov	byte ptr[si+02h],00h
	mov	byte ptr[si+03h],00h
	mov	byte ptr[si+04h],00h
	mov	byte ptr[si+05h],00h
	mov	byte ptr[si+06h],00h
	mov	byte ptr[si+07h],00h

	pop	si
	ret
cleardn	endp


delay    proc		;��ʱdelayt����
         push  cx
         mov   ecx,delayt
llll:    loop  llll  
         pop   cx
         ret
delay    endp  


delayn	proc		;����delay�ӳ�����
	call	delay
	call	delay
	call	delay
	call	delay
	call	delay

	ret
delayn	endp


decdt	proc		;��Сdelayt
	push	ax
	push	bx
	push	cx
	push	dx
	push	si

	mov	ecx,delayt
	sub	ecx,700h
	mov	delayt,ecx	

	pop	si
	pop	dx
	pop	cx
	pop	bx
	pop	ax
	ret
decdt	endp


incdt	proc		;����delayt
	push	ax
	push	bx
	push	cx
	push	dx
	push	si

	mov	ecx,delayt
	add	ecx,700h
	mov	delayt,ecx	

	pop	si
	pop	dx
	pop	cx
	pop	bx
	pop	ax
	ret
incdt	endp


code     ends
         end   start 