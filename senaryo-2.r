# Karakter kodlaması ayarları
Sys.setlocale("LC_ALL", "en_US.UTF-8")
# Windows için: Sys.setlocale("LC_ALL", "Turkish")

# Grafik aygıtını UTF-8 destekli olarak ayarla
options(encoding = "UTF-8")

# Pareto rastgele sayı üretici fonksiyonu
pareto_rv <- function(n, alpha, xm) {
  xm / (runif(n)^(1 / alpha))
}

# Parametreler
set.seed(456)
n_iter <- 10000            # Simülasyon sayısı
size <- 20                 # Negatif binomial size parametresi
prob <- 0.8                # Negatif binomial başarı olasılığı
alpha <- 2.5               # Pareto şekil parametresi
xm <- 5000                 # Pareto ölçek parametresi (minimum değer)

# Simülasyon
toplam_hasarlar <- replicate(n_iter, {
  n_claims <- rnbinom(1, size = size, prob = prob)
  if (n_claims > 0) {
    sum(pareto_rv(n_claims, alpha = alpha, xm = xm))
  } else {
    0
  }
})

# Simülasyon sonuçları
ortalama_sim <- mean(toplam_hasarlar)
varyans_sim <- var(toplam_hasarlar)
q90_sim <- quantile(toplam_hasarlar, 0.90)
q995_sim <- quantile(toplam_hasarlar, 0.995)

# Analitik Hesaplamalar
# Pareto dağılımı için
EX <- ifelse(alpha > 1, alpha * xm / (alpha - 1), Inf)
VarX <- ifelse(alpha > 2, (alpha * xm^2) / ((alpha - 1)^2 * (alpha - 2)), Inf)

# Negatif binomial dağılımı için
EN <- size * (1 - prob) / prob  # Negatif binomial beklenen değeri
VarN <- size * (1 - prob) / prob^2  # Negatif binomial varyansı

# Bileşik dağılım için beklenen değer ve varyans
E_S <- EN * EX
Var_S <- EN * VarX + (EX^2) * VarN

# Risk sermayesi (99.5% VaR - Ortalama)
risk_sermayesi_sim <- q995_sim - ortalama_sim

# Sonuçları yazdır
cat("\nSenaryo 2 - Negatif Binomial + Pareto Bilesik Dagilim\n")
cat("--------------------------------------------------\n")
cat("Simulasyon Ortalama Hasar:", format(ortalama_sim, scientific = FALSE), "\n")
cat("Analitik Ortalama Hasar:", format(E_S, scientific = FALSE), "\n")
cat("\n")
cat("Simulasyon Varyans:", format(varyans_sim, scientific = FALSE), "\n")
cat("Analitik Varyans:", format(Var_S, scientific = FALSE), "\n")
cat("\n")
cat("Simulasyon 90% Yuzdelik Dilim:", format(q90_sim, scientific = FALSE), "\n")
cat("Simulasyon 99.5% Yuzdelik Dilim:", format(q995_sim, scientific = FALSE), "\n")
cat("\n")
cat("Risk Sermayesi (Simulasyon):", format(risk_sermayesi_sim, scientific = FALSE), "\n")

# PDF grafik dosyası olarak kaydet (isteğe bağlı)
pdf("senaryo2_grafikler.pdf", width=10, height=14)

# Ana dağılım grafiği - karakter kodlaması sorunlarını önlemek için
par(mfrow = c(1, 1))
hist(toplam_hasarlar, breaks = 60, probability = TRUE,
     col = "#DDEEFF", border = "white",
     main = "Senaryo 2 - Negatif Binomial + Pareto Toplam Hasar Dagilimi",
     xlab = "Toplam Hasar")

# Yoğunluk eğrisi ekle
lines(density(toplam_hasarlar, adjust = 1.5), col = "red", lwd = 2)
abline(v = q90_sim, col = "blue", lty = 2, lwd = 2)
abline(v = q995_sim, col = "darkgreen", lty = 2, lwd = 2)
abline(v = ortalama_sim, col = "purple", lty = 3, lwd = 2)

# Lejant ekle
legend("topright", 
       legend = c("Yogunluk Egrisi", "90. Yuzdelik", "99.5. Yuzdelik", "Ortalama"),
       col = c("red", "blue", "darkgreen", "purple"), 
       lty = c(1, 2, 2, 3), 
       lwd = 2)

# Negatif Binomial ve Pareto dağılımlarının görselleştirilmesi
nb_counts <- rnbinom(10000, size = size, prob = prob)
pareto_claims <- pareto_rv(10000, alpha = alpha, xm = xm)

# İki panel grafik oluştur
par(mfrow = c(2, 1))

# Hasar sayısı dağılımı
hist(nb_counts, breaks = 30, col = "#FFDDEE", border = "white",
     main = "Negatif Binomial Hasar Sayisi Dagilimi", 
     xlab = "Hasar Sayisi")

# Hasar büyüklüğü dağılımı
hist(pareto_claims, breaks = 100, col = "#EEFFDD", border = "white",
     main = "Pareto Hasar Buyuklugu Dagilimi", 
     xlab = "Hasar Buyuklugu")

# Pareto'nun uzun kuyruğunu göstermek için logaritmik ölçekte de çizelim
par(mfrow = c(1, 1))
hist(log10(pareto_claims), breaks = 50, col = "#EEFFDD", border = "white",
     main = "Pareto Hasar Buyuklugu Dagilimi (Log10 Olcegi)", 
     xlab = "Log10(Hasar Buyuklugu)")

# PDF kapama
dev.off()

# Ayrıca ekranda görüntüle - ana dağılım grafiği
par(mfrow = c(1, 1))
hist(toplam_hasarlar, breaks = 60, probability = TRUE,
     col = "#DDEEFF", border = "white",
     main = "Senaryo 2 - Negatif Binomial + Pareto Toplam Hasar Dagilimi",
     xlab = "Toplam Hasar")

# Yoğunluk eğrisi ekle
lines(density(toplam_hasarlar, adjust = 1.5), col = "red", lwd = 2)
abline(v = q90_sim, col = "blue", lty = 2, lwd = 2)
abline(v = q995_sim, col = "darkgreen", lty = 2, lwd = 2)
abline(v = ortalama_sim, col = "purple", lty = 3, lwd = 2)

# Lejant ekle
legend("topright", 
       legend = c("Yogunluk Egrisi", "90. Yuzdelik", "99.5. Yuzdelik", "Ortalama"),
       col = c("red", "blue", "darkgreen", "purple"), 
       lty = c(1, 2, 2, 3), 
       lwd = 2)

# Negatif Binomial ve Pareto dağılımlarının görselleştirilmesi - ekranda
nb_counts <- rnbinom(10000, size = size, prob = prob)
pareto_claims <- pareto_rv(10000, alpha = alpha, xm = xm)

# İki panel grafik oluştur
par(mfrow = c(2, 1))

# Hasar sayısı dağılımı
hist(nb_counts, breaks = 30, col = "#FFDDEE", border = "white",
     main = "Negatif Binomial Hasar Sayisi Dagilimi", 
     xlab = "Hasar Sayisi")

# Hasar büyüklüğü dağılımı
hist(pareto_claims, breaks = 100, col = "#EEFFDD", border = "white",
     main = "Pareto Hasar Buyuklugu Dagilimi", 
     xlab = "Hasar Buyuklugu")

# Pareto'nun uzun kuyruğunu göstermek için logaritmik ölçekte de çizelim
par(mfrow = c(1, 1))
hist(log10(pareto_claims), breaks = 50, col = "#EEFFDD", border = "white",
     main = "Pareto Hasar Buyuklugu Dagilimi (Log10 Olcegi)", 
     xlab = "Log10(Hasar Buyuklugu)")