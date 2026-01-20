use std::fs;
use std::io::{self, BufRead, BufReader, Write};
use std::os::unix::fs::PermissionsExt;
use std::process::{self, Child, Command, Stdio};
use std::thread;

// 計算を担当するエンジンの実行可能ファイルへのパス。
// このパスは実際の環境に合わせて修正する必要がある。
const ENGINE_EXECUTABLE: &str = "./algebra.exe"; // 仮のパス

pub struct Engine {
    child: Child,
}

impl Engine {
    pub fn spawn(config_path: &str) -> Result<Self, io::Error> {
        let metadata = fs::metadata(ENGINE_EXECUTABLE)
            .map_err(|e| io::Error::new(e.kind(), format!("Failed to get metadata for '{}': {}. Current working directory is '{}'", ENGINE_EXECUTABLE, e, std::env::current_dir().unwrap().display())))?;
        let permissions = metadata.permissions();

        if permissions.mode() & 0o111 == 0 {
            return Err(io::Error::new(
                io::ErrorKind::PermissionDenied,
                format!(
                    "Engine executable '{}' is not executable. Please grant execute permission (e.g., `chmod +x {}`).",
                    ENGINE_EXECUTABLE,
                    ENGINE_EXECUTABLE
                ),
            ));
        }

        let mut child = Command::new(ENGINE_EXECUTABLE) // 実行可能ファイルを指定
            .arg(config_path) // 設定ファイルパスを引数として渡す
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn()?;

        if let Some(stderr) = child.stderr.take() {
            thread::spawn(move || {
                let reader = BufReader::new(stderr);
                for line in reader.lines() {
                    eprintln!("[engine stderr] {}", line.unwrap_or_else(|e| e.to_string()));
                }
            });
        }
        
        Ok(Engine { child })
    }

    pub fn query(&mut self, input: &str) -> String {
        // eprintln!("[cli debug] Querying with: '{}'", input);
        let stdin = self.child.stdin.as_mut().unwrap();

        if let Err(e) = writeln!(stdin, "{}", input) {
            // eprintln!("[cli debug] Failed to write to stdin: {}", e);
            return format!("Error writing to engine: {}", e);
        }
        // eprintln!("[cli debug] Wrote to stdin.");

        if let Err(e) = stdin.flush() {
            // eprintln!("[cli debug] Failed to flush stdin: {}", e);
            return format!("Error flushing stdin: {}", e);
        }
        // eprintln!("[cli debug] Flushed stdin. Reading from stdout...");

        let mut reader = BufReader::new(self.child.stdout.as_mut().unwrap());
        
        let mut full_response = String::new();
        let mut buffer = String::new();

        loop {
            buffer.clear();
            // eprintln!("[cli debug] Calling read_line...");
            let _bytes_read = match reader.read_line(&mut buffer) {
                Ok(0) => {
                    // eprintln!("[cli debug] read_line returned 0 bytes (EOF).");
                    break;
                }
                Ok(n) => {
                    // eprintln!("[cli debug] read_line read {} bytes.", n);
                    n
                }
                Err(e) => {
                    // eprintln!("[cli debug] read_line returned an error: {}", e);
                    break;
                }
            };

            // eprintln!("[cli debug] read buffer: '{}'", buffer.trim_end());
            full_response.push_str(&buffer);

            // LaTeXの終端マーカー `\end{document}` を検出したらループを抜ける
            // trim() を使用せず、正確な文字列で判定する
            if buffer.contains("\\end{document}") {
                // eprintln!("[cli debug] Found end marker.");
                break;
            }
        }

        // eprintln!("[cli debug] Exited read loop. Full response length: {}", full_response.len());
        full_response.trim_end().to_string()
    }
}

impl Drop for Engine {
    fn drop(&mut self) {
        let _ = self.child.kill();
    }
}
