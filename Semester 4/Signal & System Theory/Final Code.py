import numpy as np
import matplotlib.pyplot as plt
from scipy.io import wavfile
from scipy.fftpack import fft

#1 load and prepare the audio signal
fs, data = wavfile.read("audio_name.wav")

if len(data.shape) > 1: #make sure its mono (1 dimension)
    data = data.mean(axis=1) # average left and right columns into one

data = data.astype(float) / np.max(np.abs(data)) #y values -1 to 1

#2 extract query clip
starttimeofclip = 5.0
clipduration = 3.0

startindex = int(starttimeofclip* fs)
endindex = int((starttimeofclip + clipduration) * fs)

query_clip = data[startindex:endindex]

#3 frequency domain conversion
def get_fft(signal_segment):
    N = len(signal_segment)
    fft_val = fft(signal_segment)
    magnitude = 2/N * np.abs(fft_val[0:int(N/2)])
    return magnitude

query_fft = get_fft(query_clip)

#4 audio matching (sliding window)
window_len = len(query_clip)
stepsize = int(fs * 0.2)
similarity_scores = []
timestamps = []

for i in range(0, len(data) - window_len, stepsize):
    segment = data[i : i + window_len]
    segment_fft = get_fft(segment)
    
    # similarity score calculation using dot product
    dot_prod = np.dot(query_fft, segment_fft)
    norm_prod = np.linalg.norm(query_fft) * np.linalg.norm(segment_fft)
    score = dot_prod / norm_prod
    
    similarity_scores.append(score)
    timestamps.append(i / fs)

#6 detect best match
best_match_idx = np.argmax(similarity_scores)
detected_time = timestamps[best_match_idx]

# printing expected outputs
print(f"Sampling frequency: {fs} Hz")
print(f"Length of full signal: {len(data)/fs:.2f} s")
print(f"Clip length: {clipduration} s")
print(f"Original clip position: {starttimeofclip} s")
print(f"Detected position: {detected_time:.2f} s")
print(f"Best similarity score: {max(similarity_scores):.4f}")

#7 visualizations
plt.figure(figsize=(12, 10))

# full signal (time domain)
plt.subplot(5, 1, 1)
time_axis = np.linspace(0, len(data)/fs, len(data))
plt.plot(time_axis, data)
plt.title("Full Signal (Time Domain)")
plt.xlabel("Time (s)")

# clip signal (time domain)
plt.subplot(5, 1, 2)
clip_time_axis = np.linspace(0, len(query_clip)/fs, len(query_clip))
plt.plot(clip_time_axis, query_clip)
plt.title("Clip Signal (Time Domain)")
plt.xlabel("Time (s)")

# clip frequency spectrum
plt.subplot(5, 1, 3)
freq_axis = np.linspace(0, fs/2, len(query_fft))
plt.plot(freq_axis, query_fft)
plt.title("Clip Frequency Spectrum (FFT Magnitude)")
plt.xlabel("Frequency (Hz)")

# similarity score vs. time
plt.subplot(5, 1, 4)
plt.plot(timestamps, similarity_scores)
plt.axvline(x=detected_time, color='r', linestyle='--', label="Detected")
plt.axvline(x=starttimeofclip, color='g', alpha=0.5, label="Actual")
plt.title("Similarity Score vs. Time")
plt.legend()

# original Clip vs detected Segment
plt.subplot(5, 1, 5)
detected_segment = data[int(detected_time*fs) : int(detected_time*fs)+window_len]
plt.plot(query_clip, label="Original Clip", alpha=0.8)
plt.plot(detected_segment, label="Detected Segment", alpha=0.6)
plt.title("Original Clip vs. Detected Segment")
plt.legend()

plt.show()