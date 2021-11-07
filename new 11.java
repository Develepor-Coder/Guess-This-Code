/*
 *  This gist is from 2017 and no effort is made to keep it
 *  up-to-date, so if you're looking for an accurate bot
 *  for reference, please consider looking elsewhere.
 */

package com.rezzedup.deletebot;

import net.dv8tion.jda.core.AccountType;
import net.dv8tion.jda.core.JDA;
import net.dv8tion.jda.core.JDABuilder;
import net.dv8tion.jda.core.Permission;
import net.dv8tion.jda.core.entities.Message;
import net.dv8tion.jda.core.entities.TextChannel;
import net.dv8tion.jda.core.events.message.MessageReceivedEvent;
import net.dv8tion.jda.core.hooks.ListenerAdapter;

import java.time.OffsetDateTime;
import java.time.temporal.ChronoUnit;
import java.util.List;

public class Bot extends ListenerAdapter
{
    public static void main(String[] args)
    {
        if (args.length <= 0)
        {
            System.out.println("Expected bot token as first argument.");
            return;
        }
    
        JDA jda;
        
        try
        {
            jda = new JDABuilder(AccountType.BOT).setToken(args[0]).buildBlocking();
        }
        catch (Exception e)
        {
            e.printStackTrace();
            return;
        }
        
        new Bot(jda);
    }
    
    private final JDA jda;
    
    boolean isWorking = false;
    
    public Bot(JDA jda)
    {
        this.jda = jda;
        jda.addEventListener(this);
    }
    
    @Override
    public void onMessageReceived(MessageReceivedEvent event)
    {
        TextChannel channel = event.getTextChannel();
        
        boolean isClearCommand = event.getMember().getPermissions(channel).contains(Permission.ADMINISTRATOR)
            && event.getMessage().getContent().matches("(?i)^(clear|delete|prune|purge)( all messages in)? this channel$");
        
        if (isClearCommand)
        {
            clear(channel);
        }
    }
    
    public void clear(TextChannel channel)
    {
        if (isWorking)
        {
            channel.sendMessage("I'm busy right now...").queue();
            return;
        }
        
        isWorking = true;
        
        OffsetDateTime twoWeeksAgo = OffsetDateTime.now().minus(2, ChronoUnit.WEEKS);
        
        System.out.println("Deleting messages in channel: " + channel);
        
        new Thread(() -> 
        {
            while (isWorking)
            {
                List<Message> messages = channel.getHistory().retrievePast(50).complete();
                
                messages.removeIf(m -> m.getCreationTime().isBefore(twoWeeksAgo));
        
                if (messages.isEmpty())
                {
                    isWorking = false;
                    System.out.println("Done deleting: " + channel);
                    return;
                }
        
                messages.forEach(m -> System.out.println("Deleting: " + m));
                channel.deleteMessages(messages).complete();
            }
        })
        .start();
    }
}