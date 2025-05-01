# Karakter kodlaması ayarları
Sys.setlocale("LC_ALL", "en_US.UTF-8")
# Windows için: Sys.setlocale("LC_ALL", "Turkish")

# Grafik aygıtını UTF-8 destekli olarak ayarla
options(encoding = "UTF-8")

# Parametreler
set.seed(42)
num_simulations <- 10000    # Simülasyon sayısı
lambda <- 5000              # Poisson dağılımı parametresi (hasar sayısı)
beta <- 1/10000             # Exponential dağılımı rate parametresi (hasarın büyüklüğü)

# Simülasyon başlat
loss_totals <- numeric(num_simulations)

for (i in 1:num_simulations) {
  claim_count <- rpois(1, lambda)
  if (claim_count > 0) {
    losses <- rexp(claim_count, rate = beta)  # rate = 1/θ = β
    loss_totals[i] <- sum(losses)
  }
  # claim_count == 0 ise zaten 0 atanmış durumda
}

# Simülasyon sonuçları
mean_sim <- mean(loss_totals)
var_sim <- var(loss_totals)
p90_sim <- quantile(loss_totals, 0.90)
p995_sim <- quantile(loss_totals, 0.995)

# Teorik (analitik) sonuçlar
expected_loss <- lambda * (1/beta)  # E[S] = E[N] * E[X] = λ * (1/β)
var_theoretical <- lambda * (1/beta)^2  # Var[S] = E[N] * Var[X] = λ * (1/β)²

# Not: Bileşik Poisson-Üstel dağılımı için, toplam hasar Gamma dağılımına yaklaşır
# shape = lambda, scale = 1/beta
p90_theoretical <- qgamma(0.90, shape = lambda, scale = 1/beta)
p995_theoretical <- qgamma(0.995, shape = lambda, scale = 1/beta)

# Risk sermayesi
rc_simulated <- p995_sim - mean_sim
rc_theoretical <- p995_theoretical - expected_loss

# Sonuçları yazdır
cat("Senaryo 1 - Poisson + Exponential\n")
cat("----------------------------------\n")
cat("Simulasyon Ortalama Hasar:", format(mean_sim, scientific = FALSE), "\n")
cat("Analitik Ortalama Hasar:", format(expected_loss, scientific = FALSE), "\n")
cat("\n")
cat("Simulasyon Varyans:", format(var_sim, scientific = FALSE), "\n")
cat("Analitik Varyans:", format(var_theoretical, scientific = FALSE), "\n")
cat("\n")
cat("Simulasyon 90% Yuzdelik Dilim:", format(p90_sim, scientific = FALSE), "\n")
cat("Analitik 90% Yuzdelik Dilim:", format(p90_theoretical, scientific = FALSE), "\n")
cat("\n")
cat("Simulasyon 99.5% Yuzdelik Dilim:", format(p995_sim, scientific = FALSE), "\n")
cat("Analitik 99.5% Yuzdelik Dilim:", format(p995_theoretical, scientific = FALSE), "\n")
cat("\n")
cat("Risk Sermayesi (Simulasyon):", format(rc_simulated, scientific = FALSE), "\n")
cat("Risk Sermayesi (Analitik):", format(rc_theoretical, scientific = FALSE), "\n")

# Poisson ve Exponential dağılımlarından örnekler oluştur
poisson_counts <- rpois(10000, lambda)
exp_claims <- rexp(10000, rate = beta)

# PDF grafik dosyası olarak kaydet
pdf("senaryo1_grafikler.pdf", width=10, height=14)

# Ana dağılım grafiği
par(mfrow = c(1, 1))
hist(loss_totals, breaks = 60, probability = TRUE,
     col = "#DDEEFF", border = "white",
     main = "Senaryo 1 - Poisson + Exponential Toplam Hasar Dagilimi",
     xlab = "Toplam Hasar")

# Yoğunluk eğrisi ekle
lines(density(loss_totals), col = "red", lwd = 2)
abline(v = p90_sim, col = "blue", lty = 2, lwd = 2)
abline(v = p995_sim, col = "darkgreen", lty = 2, lwd = 2)
abline(v = mean_sim, col = "purple", lty = 3, lwd = 2)

# Lejant ekle
legend("topright", 
       legend = c("Yogunluk Egrisi", "90. Yuzdelik", "99.5. Yuzdelik", "Ortalama"),
       col = c("red", "blue", "darkgreen", "purple"), 
       lty = c(1, 2, 2, 3), 
       lwd = 2)

# Poisson ve Exponential dağılımlarının görselleştirilmesi
par(mfrow = c(2, 1))

# Hasar sayısı dağılımı (Poisson)
hist(poisson_counts, breaks = 50, col = "#FFDDEE", border = "white",
     main = "Poisson Hasar Sayisi Dagilimi", 
     xlab = "Hasar Sayisi")

# Hasar büyüklüğü dağılımı (Exponential)
hist(exp_claims, breaks = 100, col = "#EEFFDD", border = "white",
     main = "Exponential Hasar Buyuklugu Dagilimi", 
     xlab = "Hasar Buyuklugu")

# Exponential dağılımını daha iyi görmek için logaritmik ölçekte
par(mfrow = c(1, 1))
hist(log10(exp_claims), breaks = 50, col = "#EEFFDD", border = "white",
     main = "Exponential Hasar Buyuklugu Dagilimi (Log10 Olcegi)", 
     xlab = "Log10(Hasar Buyuklugu)")

# QQ Plot - Normal dağılım ile karşılaştırma
qqnorm(loss_totals, main = "Toplam Hasar QQ Plot (Normal Dagilim)")
qqline(loss_totals, col = "red")

# PDF kapama
dev.off()

# Ayrıca ekranda görüntüle - ana dağılım grafiği
par(mfrow = c(1, 1))
hist(loss_totals, breaks = 60, probability = TRUE,
     col = "#DDEEFF", border = "white",
     main = "Senaryo 1 - Poisson + Exponential Toplam Hasar Dagilimi",
     xlab = "Toplam Hasar")

# Yoğunluk eğrisi ekle
lines(density(loss_totals), col = "red", lwd = 2)
abline(v = p90_sim, col = "blue", lty = 2, lwd = 2)
abline(v = p995_sim, col = "darkgreen", lty = 2, lwd = 2)
abline(v = mean_sim, col = "purple", lty = 3, lwd = 2)

# Lejant ekle
legend("topright", 
       legend = c("Yogunluk Egrisi", "90. Yuzdelik", "99.5. Yuzdelik", "Ortalama"),
       col = c("red", "blue", "darkgreen", "purple"), 
       lty = c(1, 2, 2, 3), 
       lwd = 2)

# Poisson ve Exponential dağılımlarının görselleştirilmesi - ekranda
par(mfrow = c(2, 1))

# Hasar sayısı dağılımı (Poisson)
hist(poisson_counts, breaks = 50, col = "#FFDDEE", border = "white",
     main = "Poisson Hasar Sayisi Dagilimi", 
     xlab = "Hasar Sayisi")

# Hasar büyüklüğü dağılımı (Exponential)
hist(exp_claims, breaks = 100, col = "#EEFFDD", border = "white",
     main = "Exponential Hasar Buyuklugu Dagilimi", 
     xlab = "Hasar Buyuklugu")

# Exponential dağılımını daha iyi görmek için logaritmik ölçekte
par(mfrow = c(1, 1))
hist(log10(exp_claims), breaks = 50, col = "#EEFFDD", border = "white",
     main = "Exponential Hasar Buyuklugu Dagilimi (Log10 Olcegi)", 
     xlab = "Log10(Hasar Buyuklugu)")

# QQ Plot - Normal dağılım ile karşılaştırma
qqnorm(loss_totals, main = "Toplam Hasar QQ Plot (Normal Dagilim)")
qqline(loss_totals, col = "red")