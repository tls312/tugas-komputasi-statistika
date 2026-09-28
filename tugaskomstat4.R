# ==============================================================================
# TUGAS: PROBABILITAS DAN STATISTIKA - DISTRIBUSI DISKRIT
# Nama       : Bilhadi Muhammad
# NIM        : 3338250028
# ==============================================================================

# Mengaktifkan library untuk visualisasi modern (jika belum ada, install dengan install.packages("ggplot2"))
library(ggplot2)

# ------------------------------------------------------------------------------
# KASUS 1: DISTRIBUSI POISSON
# Soal: Rata-rata pelanggan = 3 orang/jam. Hitung P(X >= 5).
# ------------------------------------------------------------------------------

# Definisi Parameter
rata_rata_poisson <- 3  # Nilai rata-rata kejadian (lambda)

# Menghitung peluang P(X >= 5) menggunakan komplemen: 1 - P(X <= 4)
peluang_poisson_ge5 <- 1 - ppois(q = 4, lambda = rata_rata_poisson)

# Output Hasil
cat("=== KASUS 1: DISTRIBUSI POISSON ===\n")
cat(sprintf("Probabilitas P(X >= 5) dengan lambda = %d adalah: %.5f\n\n", 
            rata_rata_poisson, peluang_poisson_ge5))


# ------------------------------------------------------------------------------
# KASUS 2: DISTRIBUSI HIPERGEOMETRIK
# Soal: Pop = 100 bola, Sukses (Merah) = 20, Sampel = 10 tanpa pengembalian.
# ------------------------------------------------------------------------------

# Parameter Distribusi
pop_total     <- 100 # N (Total Populasi)
sukses_pop    <- 20  # M atau K (Jumlah elemen sukses di populasi)
gagal_pop     <- pop_total - sukses_pop # N - K (Jumlah elemen gagal di populasi)
ukuran_sampel <- 10  # n (Banyaknya sampel yang diambil)

# Menentukan batas domain k (nilai sukses yang mungkin terambil)
k_min <- max(0, ukuran_sampel - gagal_pop)
k_max <- min(ukuran_sampel, sukses_pop)
domain_k <- k_min:k_max

# Menghitung Probability Mass Function (PMF)
peluang_hiper <- dhyper(x = domain_k, m = sukses_pop, n = gagal_pop, k = ukuran_sampel)

# Menyusun Tabel Distribusi Probabilitas menggunakan format matriks/tabel bersih
cat("=== KASUS 2: DISTRIBUSI HIPERGEOMETRIK ===\n")
cat("Tabel Distribusi Peluang Kumulatif & PMF:\n")
tabel_distribusi <- data.frame(
  `Jumlah Bola Merah (k)` = domain_k,
  `Peluang P(X=k)` = round(peluang_hiper, 6),
  check.names = FALSE
)
print(tabel_distribusi, row.names = FALSE)

# Perhitungan Nilai Harapan (Ekspektasi) dan Varians Teoretis
ekspektasi_teoretis <- ukuran_sampel * (sukses_pop / pop_total)
varians_teoretis    <- ukuran_sampel * (sukses_pop / pop_total) * 
  (1 - sukses_pop / pop_total) * 
  ((pop_total - ukuran_sampel) / (pop_total - 1))

cat("\nUkuran Statistik Teoretis:\n")
cat(sprintf("- Ekspektasi E[X] : %.4f\n", ekspektasi_teoretis))
cat(sprintf("- Varians Var(X)  : %.4f\n\n", varians_teoretis))


# ------------------------------------------------------------------------------
# KASUS 3: SIMULASI BINOMIAL VS PMF TEORETIS (GGPLOT2)
# Soal: Simulasi 1.000 percobaan Binomial (n = 15, p = 0.4) & bandingkan plotnya.
# ------------------------------------------------------------------------------

# Mengunci random generator agar hasil simulasi konsisten
set.seed(2026) 

# Parameter Binomial
total_simulasi <- 1000
jumlah_trial   <- 15
prob_sukses    <- 0.4

# 1. Menghasilkan data acak hasil simulasi empiris
hasil_simulasi <- rbinom(n = total_simulasi, size = jumlah_trial, prob = prob_sukses)
df_simulasi <- data.frame(Sukses = hasil_simulasi)

# 2. Menghitung PMF Teoretis untuk garis pembanding
nilai_x <- 0:jumlah_trial
pmf_aktual <- dbinom(x = nilai_x, size = jumlah_trial, prob = prob_sukses)
df_teoretis <- data.frame(Sukses = nilai_x, Peluang = pmf_aktual)

# 3. Visualisasi Menggunakan GGPLOT2 (Lebih Modern dibanding plot bawaan R)
cat("=== KASUS 3: SIMULASI BINOMIAL ===\n")
cat("Generating plot menggunakan ggplot2...\n")

plot_perbandingan <- ggplot() +
  # Membuat histogram dari data empiris hasil simulasi
  geom_histogram(data = df_simulasi, aes(x = Sukses, y = after_stat(density), fill = "Simulasi Empiris"),
                 binwidth = 1, color = "white", alpha = 0.7, center = 0) +
  # Menambahkan titik dan garis untuk PMF Teoretis
  geom_line(data = df_teoretis, aes(x = Sukses, y = Peluang, color = "PMF Teoretis"), linewidth = 1) +
  geom_point(data = df_teoretis, aes(x = Sukses, y = Peluang, color = "PMF Teoretis"), size = 2.5) +
  # Kustomisasi Skala dan Tema Visual
  scale_x_continuous(breaks = 0:jumlah_trial) +
  scale_fill_manual(values = c("Simulasi Empiris" = "#4ea8de")) +
  scale_color_manual(values = c("PMF Teoretis" = "#e63946")) +
  labs(
    title = "Perbandingan Distribusi Binomial",
    subtitle = paste0("Simulasi Empiris (N = ", total_simulasi, ") vs Teoretis (n = ", jumlah_trial, ", p = ", prob_sukses, ")"),
    x = "Jumlah Kejadian Sukses (k)",
    y = "Probabilitas / Densitas",
    fill = "Dataset",
    color = "Garis Panduan"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "bottom"
  )

# Menampilkan grafik ke layar
print(plot_perbandingan)
