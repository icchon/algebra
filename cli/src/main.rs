mod engine;
mod parser;
mod session;

use session::Session;

fn main() {
    if let Err(e) = run() {
        eprintln!("Application error: {}", e);
    }
}

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
        "mode" if words.len() > 1 => Ok(Command::Switch(words[1].to_string())),
        "mode" => Err(CommandParseError::InvalidArguments(
            "switch command requires a mode (e.g., 'switch linear')".into(),
        )),
        _ => Ok(Command::Expr(line.to_string())),
    }
}

fn run() -> Result<(), Box<dyn std::error::Error>> {
    let mut rl = rustyline::DefaultEditor::new()?;

    let mut session = Session::new("linear")?;

    loop {
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
                    match Session::new(&mode_name) {
                        Ok(new_session) => {
                            session = new_session;
                        }
                        Err(e) => eprintln!("Failed to switch mode: {}", e),
                    }
                }
                Command::Expr(expr) => {
                    match session.parser.parse(&expr) {
                        Ok(parsed_data) => {
                            let line = parsed_data.lines().map(|s| s.trim()).collect::<String>();   
                            // println!("{}", line);
                            let result = session.engine.query(&line);
                            println!("\n\n{}\n\n", result);
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