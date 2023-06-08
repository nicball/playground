/**
 * Welcome to Cloudflare Workers! This is your first worker.
 *
 * - Run `npx wrangler dev src/index.js` in your terminal to start a development server
 * - Open a browser tab at http://localhost:8787/ to see your worker in action
 * - Run `npx wrangler publish src/index.js --name my-worker` to publish your worker
 *
 * Learn more at https://developers.cloudflare.com/workers/
 */

export default {
  async fetch(request, env, ctx) {
    try {
      let json = await request.json();
      console.log(json);
      let new_members = json.message.new_chat_members;
      let chat_id = json.message.chat.id;
      return new Response(
        JSON.stringify({
          method: "promoteChatMember",
          chat_id: chat_id,
          user_id: new_members[0].id,
          is_anonymous: true
        }),
        {
          headers: { "Content-Type": "application/json;charset=UTF-8" }
        }
      );
    }
    catch (e) {
      console.log(e);
    }
    return new Response("Hello World!");
  },
};
