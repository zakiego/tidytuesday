# Lib ----

library(nusandata)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(scales)
library(hrbrthemes)
library(lubridate)

# Data ----

haji <- brk_haji

haji.bersih <- haji %>%
  filter(str_detect(nama_daerah, "Prov")) %>% 
  arrange(desc(batas_tahun)) %>% 
  top_n(10) %>% 
  mutate(nama_daerah = str_remove_all(nama_daerah, "Prov. ")) %>% 
  select(-jumlah_lunas_tunda) %>% 
  pivot_longer(cols = c("kuota", "jumlah_pendaftaran"),
               names_to = "kategori",
               values_to = "jumlah")

haji.bersih$batas_tahun[haji.bersih$kategori == "kuota"] <- NA

haji.bersih$nama_daerah[haji.bersih$nama_daerah == "Sumatera Utara"] <- "Sumut"
haji.bersih$nama_daerah[haji.bersih$nama_daerah == "Sumatera Selatan"] <- "Sumsel"
haji.bersih$nama_daerah[haji.bersih$nama_daerah == "Nusa Tenggara Barat"] <- "NTB"
haji.bersih$nama_daerah[haji.bersih$nama_daerah == "Kalimantan Selatan"] <- "Kalsel"
haji.bersih$nama_daerah[haji.bersih$nama_daerah == "Jawa Timur"] <- "Jatim"
haji.bersih$nama_daerah[haji.bersih$nama_daerah == "Jawa Tengah"] <- "Jateng"

angka <-  as.data.frame(c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20))
names(angka) <- 'urutan'

haji.bersih$baris <- angka$urutan

# Grafik ----

haji.bersih %>%
  ggplot(
    aes(
      y = reorder(nama_daerah,desc(baris)),
      x = jumlah,
      group = kategori,
      fill = kategori,
      color = kategori,
      label = batas_tahun)) +
  
  geom_bar(stat = 'identity',
           position = 'dodge',
           width = 0.8) +
  
  geom_text(hjust = -0.4, # Kiri - Kanan
            vjust = 1.2,
            size = 5) +
  
  theme_ft_rc() +
  
  xlab("") + ylab("") +
  
  labs(title = "Lama Antrian Haji", # Judul
       subtitle = "Kalsel memiliki waktu tunggu terlama \nkarena pendaftarnya banyak, namun kuotanya sedikit") + 
  
  theme(
    legend.justification = "top", # Posisi legenda, alternatif dari legend.position
    legend.title = element_blank(), # Menghilangkan judul legenda
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank()) +
  
  scale_color_manual(
    values = c("#bb86fc", "#370083"), # Colour Batang
    labels = c("Jumlah Pendaftar", "Kuota")) +
  
  scale_fill_manual(
    values = c("#bb86fc", "#370083"), # Fill Batang
    labels = c("Jumlah Pendaftar", "Kuota")) +
  
  scale_x_continuous(labels = comma,
                     limits = c(0, 600000)) +
    
  ggsave("Lama Antrian Haji.png",
         height = 8, width =10)  
