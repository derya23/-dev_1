#####    YILLARA GÖRE HAL FİYATLARI  #####

hf<-read.csv("Affordable_Housing_by_Town_2011-2022.csv")
library(dplyr)
library(tidyr)
library(ggplot2)
install.packages("intsvy", repos = "http://cran.us.r-project.org")
#hf verisetinin boyutlarının yazdırılması
dim(hf)
hf

#hf veri setine ilişkin betimsel istatiksellerin yazdırılması
summary(hf)

#hükümet destekli 200 den büyük konut (Government.Assisted)
hf[hf[5]>200,]

#2020 yılındaki kiracı kiralama yardımı(tenant.rental.assistance) 150 den büyük
#olanlar

hf[hf[1]==2020 & hf[6]>150,]

#hf veri setinin yapısının incelenmesi
str(hf)

#hf veri setinin incelenmesi
attributes(hf)


#hf veri setinde single.family.chfa..usda.mortgages a göre büyükten küçüğe sıralama
hf[order(hf$Single.Family.CHFA..USDA.Mortgages , decreasing=T),]

#hf veri setini tibble() a çevirme
as_tibble(hf)

#select() ile yeni nesne oluşturma

hf1<-select(hf, Town, Deed.Restricted.Units)
hf1

#to ile başlayan sütunların seçilmesi
select(hf, starts_with("To"))

hf%>%
  select(Town:Total.Assisted.Units)%>%
  mutate(toplam=Deed.Restricted.Units+Total.Assisted.Units)%>%
  names()

#hükümet destekli konutların ortalamasının bulunması
hf[,mean(Government.Assisted)]
hf[,.(mean(Government.Assisted))]

#grafik

plot(hf$X2010.Census.Units)

#uygun fiyatlı ve desteklenen konut grafiği
summary(hf$Percent.Affordable)# desteklenen konut
summary(hf$Total.Assisted.Units) #uygun fiyatlı konut
plot( hf$Total.Assisted.Units, hf$Percent.Affordable,xlim=c(-5,30000), 
     ylim=c(-5,50), xlab="Desteklenen Konut", ylab="Uygun Fiyatlı Konut")

