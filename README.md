# Hasar Modelleme: Bileşik Dağılım Simülasyonları

Bu proje, sigorta ve risk yönetimi bağlamında iki farklı **bileşik dağılım modelini** kullanarak toplam hasarın simülasyonunu, görselleştirmesini ve analitik analizini gerçekleştirmektedir.

## 📊 Modeller

### 🟢 Senaryo 1: Poisson + Exponential
- **Hasar sayısı:** Poisson dağılımı (λ = 10⁴)
- **Hasar büyüklüğü:** Exponential dağılım (β = 5000)
- **Analitik formüller:** E[S] = λ * E[X], Var[S] = λ * Var[X]

### 🔵 Senaryo 2: Negative Binomial + Pareto
- **Hasar sayısı:** Negative Binomial (size = 20, prob = 0.8)
- **Hasar büyüklüğü:** Pareto dağılımı (α = 2.5, xm = 5000)
- **Uzun kuyruklu yapı:** Pareto dağılımı büyük hasarları daha olası kılar.

## 🚀 İçerik

Proje aşağıdaki aşamaları içerir:

1. **Simülasyon:** 10.000 iterasyon ile toplam hasar üretimi
2. **Görselleştirme:**
   - Histogram + yoğunluk eğrisi
   - 90% ve 99.5% yüzdelik çizgileri
   - Logaritmik histogramlar (Pareto için)
3. **İstatistiksel Analiz:**
   - Ortalama ve varyans (simülasyon ve analitik)
   - Yüzdelik dilimler (VaR)
   - Risk sermayesi hesaplaması
4. **Karşılaştırmalı analiz:** Simülasyon ile teorik değerlerin uyumu

## 🧪 Kullanım

R yüklü olduktan sonra script'leri çalıştırabilirsiniz:

```r
source("senaryo-1.r")
source("senaryo-2.r")
