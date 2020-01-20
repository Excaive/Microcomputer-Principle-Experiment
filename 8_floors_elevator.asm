;386ex MASTER /SLAVE  8259A INT ,MASTER=MIR5,SLAVE=SIR0  MASTER INT TYPE=30H-37H;SLAVE INT TYPE =70H-77H
.model small
.386

;~~~~~~~~~~~数据段~~~~~~~~~~~~~
data   segment
	i8255_a    equ  200h
	o8255_b    equ  201h
	o8255_c    equ  202h
	i8255_kz   equ  203h
      
	i8253_0    equ  210h
	i8253_1    equ  211h
	i8253_kz   equ  213h

	iled_d     equ	220h	;led段码口地址
	iled_w     equ	221h	;led位码口地址

	djcode     db   01h,03h,02h,06h,04h,0ch,08h,09h	;步进代码
	djp        dw   0000h				;步进代码指针 bx
	loopn      dw	20	;电机运行半圈需循环步进代码次数

	direc      db   00h	;电梯运行方向 0：停 1：下 2：上
	floor      db   00h     ;当前电梯楼层
	floor_up   db   00h,00h,00h,00h,00h,00h,00h,00h ;上行停靠楼层
	floor_dn   db   00h,00h,00h,00h,00h,00h,00h,00h ;下行停靠楼层
	next       db   00h	;下一停靠楼层
	tflag      db	00h	;转向标志
	bj         db   00h	;是否报警 0：否 1：是

	keyin      db	00000000b	;读入的按键信号
	keynew     db	00000000b	;本次读入的按键信号与上次相比由0变1的楼层
	keylist    db	00000001b,00000010b,00000100b,00001000b,00010000b,00100000b,01000000b,10000000b	;备用信号列表

	delayt     dd	10000h	;延迟时间
	djdelayt   dd	10000h	;电机匀速旋转延迟时间

	ledfloor   db   06h,5bh,4fh,66h,6dh,7dh,07h,7fh,67h	;led楼层代码列表
	leddirec   db	00h,1ch,23h				;led方向代码列表
	lflag      db	00h	;led数码管用显示标志
	lcycle     db	10h	;led数码管用循环变量

	rflag      db	00h	;扬声器用频率标志 0：关 4：门铃音叮(3) 5：门铃音咚(1) 6：警报音(6)
	rcycle     db	00h	;扬声器用循环变量
	rlevel     db	00000000b	;扬声器用电平标志
	
data   ends


;~~~~~~~~~~~主程序~~~~~~~~~~~~~~~   
code segment  
         assume cs:code,ds:data 
start:   cli
         mov   ax,data
         mov   ds,ax
         mov   es,ax  
;------- 8255 init---------------  
	 mov   dx,i8255_kz
         mov   al,90h       ;设置8255控制字,A口输入,B口输出,(C口输出)
         out   dx,al
;------- 8254 init--------------- 每250us检测一次按键   
         mov   dx,i8253_kz    
         mov   al,36h       ;设8254通道0工作方式2
         out   dx,al
         mov   ax,25        ;写入循环计数初值75
         mov   dx,i8253_0
         out   dx,al        ;先写入低字节
         mov   al,ah
         out   dx,al        ;后写入高字节
         mov   dx,i8253_kz
         mov   al,76h       ;设8254通道1工作方式2
         out   dx,al
         mov   ax,10        ;写入循环计数初值10
         mov   dx,i8253_1
         out   dx,al        ;先写低字节
         mov   al,ah
         out   dx,al        ;后写高字节
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
  
main_0:				;电梯静止
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


main_up:			;电梯上行
	mov	direc,02h
	mov	tflag,00h
 mu_new:		;启动
	call	tenext
	mov	cl,next
	mov	bl,floor
	cmp	bl,cl
	je	mu_z

	call	spd_up
	jmp	mu_su
 mu_loop:		;x层
	mov	bl,floor
	inc	bl
	mov	floor,bl
	call	shfloor
	call	run
  mu_su:		;x.5层
	call	tenext
	mov	cl,next
	mov	dl,floor
	inc	dl
	cmp	dl,cl
	je	mu_sd
	call	run
	jmp	mu_loop

 mu_sd:			;减速停靠
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
	
 mu_z:			;退出上行
	call	clearup
	mov	ah,tflag
	cmp	ah,1
	je	main_dn
	jmp	main_0


main_dn:			;电梯下行
	mov	direc,01h
	mov	tflag,00h
 md_new:		;启动
	call	tenext
	mov	cl,next
	mov	bl,floor
	cmp	bl,cl
	je	md_z

	call	spd_up
	jmp	md_su
 md_loop:		;x层
	mov	bl,floor
	dec	bl
	mov	floor,bl
	call	shfloor
	call	run
  md_su:		;x.5层
	call	tenext
	mov	cl,next
	mov	dl,floor
	dec	dl
	cmp	dl,cl
	je	md_sd
	call	run
	jmp	md_loop

 md_sd:			;减速停靠
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

 md_z:			;退出下行
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
m_0:			;电梯静止
	mov	dl,keynew
	and	dl,cl	
	jz	exitm_0
	cmp	bl,ah	;floor
	jg	m_0_up
	jl	m_0_dn
	jmp	exitm_0
  m_0_up:		;改变上行停留楼层
	mov	si,offset floor_up
	mov	byte ptr[si+bx],01h
	mov	dl,02h
	mov	direc,dl
	jmp	exitmint
  m_0_dn:		;改变下行停留楼层
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

m_uod:			;电梯在上升或下降
	cmp	al,2
	jne	m_d
m_u:			;电梯在上升
	mov	dl,keynew
	and	dl,cl
	jz	exitm_u
	cmp	bl,ah	;floor	
	jng	m_u_dn
  m_u_up:		;改变上行停留楼层
	mov	si,offset floor_up
	mov	byte ptr[si+bx],01h
	jmp	exitm_u	
  m_u_dn:		;改变下行停留楼层
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

m_d:			;电梯在下降
	mov	dl,keynew
	and	dl,cl
	jz	exitm_d
	cmp	bl,ah 	;floor	
	jnl	m_d_up
  m_d_dn:		;改变下行停留楼层
	mov	si,offset floor_dn
	mov	byte ptr[si+bx],01h	
	jmp	exitm_d
  m_d_up:		;改变上行停留楼层	
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


;~~~~~~~~~~~子程序~~~~~~~~~~~~~~~ 
spd_up	proc near		;电梯启动加速运行半圈
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

su_u_lp:	;加速上行半圈
	mov     al,[si+bx]
	out	dx,al
	call	delay
	call	incp
	call	decdt		
	loop	su_u_lp

	jmp	exitsu

su_d_lp:	;加速下行半圈
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


spd_dn	proc near		;电梯停靠减速运行半圈
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

sd_u_lp:	;减速上行半圈
	mov     al,[si+bx]
	out	dx,al
	call	incdt
	call	delay
	call	incp
	loop	sd_u_lp	

	jmp	exitsd

sd_d_lp:	;减速下行半圈
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



run	proc near		;电梯匀速运行半圈
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

r_u_lp:		;匀速上行半圈
	mov     al,[si+bx]
	out	dx,al
	call	delay
	call	incp
	loop	r_u_lp

	jmp	exitr

r_d_lp:		;匀速下行半圈
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


incp	proc near	;步进代码指针+1带进位（上行1/8圈）
	inc	bx
	cmp	bx,8
	jne	exitincp
	mov	bx,0
exitincp:
	ret
incp	endp


decp	proc near	;步进代码指针-1带进位（下行1/8圈）
	cmp	bx,0
	jne	exitdecp
	mov	bx,8
exitdecp:
	dec	bx
	ret
decp	endp


tenext	proc near	;测试下一停靠楼层
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


tebj	proc near	;测试当前所在楼层的开关，若开启则bj置1
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


dobj	proc near	;若bj==1则报警
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


shfloor	proc near	;将当前所在楼层用led显示出来
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


clearup	proc near	;将floor_up清零
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


cleardn	proc near	;将floor_dn清零
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


delay    proc		;延时delayt长度
         push  cx
         mov   ecx,delayt
llll:    loop  llll  
         pop   cx
         ret
delay    endp  


delayn	proc		;调用delay子程序多次
	call	delay
	call	delay
	call	delay
	call	delay
	call	delay

	ret
delayn	endp


decdt	proc		;减小delayt
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


incdt	proc		;增大delayt
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