mod engine; // engine.rs をモジュールとして宣言
mod parser;
mod session; // session.rs をモジュールとして宣言

use session::Session; // Session 構造体を使用

fn main() {
    if let Err(e) = run() {
        eprintln!("Application error: {}", e);
    }
}

// Command と CommandParseError enum は変更なし
enum Command {
    Help,
    Exit,
    Switch(String),
    Expr(String),
}

#[derive(Debug)]
enum CommandParseError {
    InvalidArguments(String),
}

fn parse_line(line: &str) -> Result<Command, CommandParseError> {
    let words: Vec<&str> = line.split_whitespace().collect();
    if words.is_empty() {
        return Err(CommandParseError::InvalidArguments("Empty input".into()));
    }

    match words[0] {
        "help" => Ok(Command::Help),
        "exit" => Ok(Command::Exit),
        "switch" if words.len() > 1 => Ok(Command::Switch(words[1].to_string())),
        "switch" => Err(CommandParseError::InvalidArguments(
            "switch command requires a mode (e.g., 'switch linear')".into(),
        )),
        _ => Ok(Command::Expr(line.to_string())),
    }
}

fn run() -> Result<(), Box<dyn std::error::Error>> {
    let mut rl = rustyline::DefaultEditor::new()?;

    // --- ここからが大きな変更点 ---

    // 1. 初期セッションを 'linear' モードで開始する
    let mut session = Session::new("linear")?;

    println!("Welcome to ComputorV2!");
    println!("Default mode: {}", session.parser.name());
    println!("Type 'switch <mode>', 'help', 'exit', or an expression.");

    loop {
        // 2. プロンプトはセッションのパーサー名から取得
        let prompt = format!("[{}] >> ", session.parser.name());
        let line = match rl.readline(&prompt) {
            Ok(line) => line,
            Err(rustyline::error::ReadlineError::Interrupted) => break, // Ctrl-C
            Err(rustyline::error::ReadlineError::Eof) => break,      // Ctrl-D
            Err(err) => {
                println!("Readline error: {:?}", err);
                break;
            }
        };

        let trimmed = line.trim();
        if trimmed.is_empty() {
            continue;
        }
        rl.add_history_entry(trimmed)?;

        match parse_line(trimmed) {
            Ok(command) => match command {
                Command::Exit => {
                    println!("Exiting.");
                    break;
                }
                Command::Help => {
                    println!("Available modes: linear, none, (calc, integrate, diff coming soon)");
                    println!("Commands: help, exit, switch <mode>");
                }
                Command::Switch(mode_name) => {
                    // 3. 'switch' で新しいセッションを生成する
                    match Session::new(&mode_name) {
                        Ok(new_session) => {
                            session = new_session;
                            println!("Switched to '{}' mode.", session.parser.name());
                        }
                        Err(e) => eprintln!("Failed to switch mode: {}", e),
                    }
                }
                Command::Expr(expr) => {
                    // 4. 式の評価： parse -> query の2段階プロセス
                    match session.parser.parse(&expr) {
                        Ok(parsed_data) => {
                            // println!("Parsed data: {}", parsed_data);
                            let line = parsed_data.lines().map(|s| s.trim()).collect::<String>();   
                            println!("{}", line);
                            let result = session.engine.query(&line);
                            println!("{}", result);
                        }
                        Err(e) => eprintln!("Parse Error: {}", e),
                    }
                }
            },
            Err(err) => match err {
                CommandParseError::InvalidArguments(msg) => {
                    eprintln!("Command error: {}", msg);
                }
            },
        };
    }
    Ok(())
}