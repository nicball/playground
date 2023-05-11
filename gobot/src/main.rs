mod go;

use crate::go::*;
use teloxide::{
    payloads::SendMessageSetters,
    prelude::*,
    types::{
        InlineKeyboardButton, InlineKeyboardMarkup, Me
    },
    utils::command::BotCommands,
    dispatching::dialogue::{self, InMemStorage}
};
use std::error::Error;

#[derive(BotCommands)]
#[command(rename_rule = "lowercase", description = "These commands are supported:")]
enum Command {
    Reveal,
    Clear
}

#[derive(Clone)]
struct GameState {
    board: Board,
    next_color: Color
}

impl GameState {
    fn new() -> Self {
        GameState { board: Board::new(9), next_color: Color::Black }
    }
}

impl Default for GameState {
    fn default() -> Self { Self::new() }
}

type MyDialogue = Dialogue<GameState, InMemStorage<GameState>>;

#[tokio::main]
async fn main() -> Result<(), Box<dyn Error>> {
    let bot = Bot::from_env();
    let handler = dialogue::enter::<Update, InMemStorage<GameState>, GameState, _>()
        .branch(Update::filter_message().endpoint(message_handler))
        .branch(Update::filter_callback_query().endpoint(callback_handler));
    Dispatcher::builder(bot, handler).dependencies(dptree::deps![InMemStorage::<GameState>::new()]).enable_ctrlc_handler().build().dispatch().await;
    Ok(())
}

async fn message_handler(bot: Bot, dialogue: MyDialogue, msg: Message, me: Me) -> Result<(), Box<dyn Error + Sync + Send>> {
    if let Some(text) = msg.text() {
        println!("Got message {}", text);
        match BotCommands::parse(text, me.username()) {
            Ok(Command::Reveal) => {
                let gs = dialogue.get().await?.unwrap();
                let kb = gen_keyboard(&gs.board);
                match bot.send_message(msg.chat.id, "Go!").reply_markup(kb).await {
                    Ok(_) => (),
                    Err(e) => { println!("{}", e); }
                };
            },
            Ok(Command::Clear) => {
                dialogue.reset().await?;
            },
            _ => println!("Unrecognised command")
        }
    }
    Ok(())
}

async fn callback_handler(bot: Bot, dialogue: MyDialogue, query: CallbackQuery) -> Result<(), Box<dyn Error + Sync + Send>> {
    if let Some(coord_str) = query.data {
        bot.answer_callback_query(query.id).await?;
        if let Some((x, y)) = parse_query_data(&coord_str) {
            let mut g = dialogue.get().await?.unwrap();
            if g.board.add_piece(g.next_color, Coord::new(x, y)) {
                g.next_color = match g.next_color {
                    Color::Black => Color::White,
                    Color::White => Color::Black
                }
            }
            if let Some(Message { id, chat, .. }) = query.message {
                let kb = gen_keyboard(&g.board);
                bot.edit_message_text(chat.id, id, "Go!").reply_markup(kb).await?;
            }
            dialogue.update(g).await?;
        }
    }
    Ok(())
}

fn parse_query_data(data: &String) -> Option<(usize, usize)> {
    let data = data.split('|').collect::<Vec<&str>>();
    if data.len() == 2 {
        data[0].parse().ok().zip(data[1].parse().ok())
    }
    else {
        None
    }
}

fn gen_keyboard(board: &Board) -> InlineKeyboardMarkup {
    let mut kb = Vec::new();
    for x in 0 .. board.size() {
        let mut row = Vec::new();
        for y in 0 .. board.size() {
            let text = match &board[Coord::new(x, y)] {
                BlockState::Empty => " ",
                BlockState::Occupied { color: Color::White, .. } => "○",
                BlockState::Occupied { color: Color::Black, .. } => "●"
            };
            row.push(InlineKeyboardButton::callback(text, x.to_string() + "|" + &y.to_string()));
        }
        kb.push(row);
    }
    InlineKeyboardMarkup::new(kb)
}
