#Bem simples para fazer o qr code no R
  #pacote do qr code
install.packages("qrcode")
  library(qrcode)
  #basta colar o link, em que ele gera lá o df
  code <-  qr_code("link de interesse")
  #e o o plot mostra o qr code de forma automática já
  plot(code)
  
  #tem a possibilidade de com a função utilizar a generate pra onde voce quer armazenar esse info
  
  generate_svg(code,filename = "pasta de destino/qr.svg")
