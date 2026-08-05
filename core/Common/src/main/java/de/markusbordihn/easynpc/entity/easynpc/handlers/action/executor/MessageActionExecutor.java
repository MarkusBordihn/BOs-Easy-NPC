/*
 * Copyright 2026 Markus Bordihn
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software and
 * associated documentation files (the "Software"), to deal in the Software without restriction,
 * including without limitation the rights to use, copy, modify, merge, publish, distribute,
 * sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies or
 * substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT
 * NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
 * DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package de.markusbordihn.easynpc.entity.easynpc.handlers.action.executor;

import de.markusbordihn.easynpc.data.action.ActionContext;
import de.markusbordihn.easynpc.data.action.ActionDataEntry;
import de.markusbordihn.easynpc.data.action.ActionUtils;
import de.markusbordihn.easynpc.data.action.MessageActionData;
import de.markusbordihn.easynpc.data.action.MessageRecipientScope;
import de.markusbordihn.easynpc.data.action.SpeechBubbleManager;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.OwnerDataCapable;
import de.markusbordihn.easynpc.network.NetworkHandlerManager;
import de.markusbordihn.easynpc.network.components.TextComponent;
import de.markusbordihn.easynpc.network.message.client.SpeechBubbleMessage;
import de.markusbordihn.easynpc.utils.TextFormattingCodes;
import de.markusbordihn.easynpc.utils.TextUtils;
import de.markusbordihn.easynpc.validator.NameValidator;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import net.minecraft.ChatFormatting;
import net.minecraft.network.chat.Component;
import net.minecraft.server.MinecraftServer;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.entity.EntitySelector;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.player.Player;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class MessageActionExecutor {

  public static final double MESSAGE_RANGE = 16.0D;
  public static final int MAX_MESSAGE_LENGTH = 256;
  protected static final Logger log = LogManager.getLogger(MessageActionExecutor.class);
  private static final Map<String, Boolean> knownPlayerNames = new ConcurrentHashMap<>();

  private MessageActionExecutor() {}

  public static void clearKnownPlayerNames() {
    knownPlayerNames.clear();
  }

  public static void execute(
      ActionDataEntry actionDataEntry, EasyNPC<?> easyNPC, ActionContext actionContext) {
    if (actionDataEntry == null || easyNPC == null) {
      return;
    }

    LivingEntity npcContext = easyNPC.getLivingEntity();
    if (npcContext == null) {
      return;
    }

    MessageActionData messageActionData = actionDataEntry.messageActionData();
    if (!messageActionData.hasAnyTarget()) {
      log.warn("Skipping message action without any output target: {}", actionDataEntry);
      return;
    }

    Component message = parseMessage(actionDataEntry, actionContext.initiator(), npcContext);
    if (message == null) {
      log.warn("Skipping message action without any text: {}", actionDataEntry);
      return;
    }

    send(
        easyNPC,
        message,
        messageActionData,
        actionContext,
        SpeechBubbleManager.DEFAULT_DURATION_TICKS);
  }

  public static void send(
      EasyNPC<?> easyNPC,
      Component message,
      MessageActionData messageActionData,
      ActionContext actionContext,
      int speechBubbleDurationTicks) {
    LivingEntity npcContext = easyNPC != null ? easyNPC.getLivingEntity() : null;
    if (npcContext == null || message == null || messageActionData == null) {
      return;
    }

    if (messageActionData.showAsSystemMessage()) {
      ServerPlayer systemMessageReceiver =
          resolveSystemMessageReceiver(easyNPC, messageActionData, actionContext);
      if (systemMessageReceiver != null) {
        systemMessageReceiver.sendSystemMessage(message);
      } else {
        log.debug("Skipping system message of {} without a receiver.", npcContext.getUUID());
      }
    }

    if (!messageActionData.showInNearbyChat() && !messageActionData.showAsSpeechBubble()) {
      return;
    }

    List<ServerPlayer> receivers = resolveReceivers(easyNPC, messageActionData, actionContext);

    if (receivers.isEmpty()) {
      return;
    }

    Component chatMessage =
        messageActionData.showInNearbyChat()
            ? Component.translatable(
                "chat.type.text", resolveSenderName(messageActionData, npcContext), message)
            : null;
    SpeechBubbleMessage speechBubbleMessage =
        messageActionData.showAsSpeechBubble()
            ? new SpeechBubbleMessage(npcContext.getUUID(), message, speechBubbleDurationTicks)
            : null;

    for (ServerPlayer receiver : receivers) {
      if (chatMessage != null) {
        receiver.sendSystemMessage(chatMessage);
      }
      if (speechBubbleMessage != null) {
        NetworkHandlerManager.sendMessageToPlayer(speechBubbleMessage, receiver);
      }
    }
  }

  private static List<ServerPlayer> resolveReceivers(
      EasyNPC<?> easyNPC, MessageActionData messageActionData, ActionContext actionContext) {
    LivingEntity npcContext = easyNPC.getLivingEntity();
    if (isDistanceEvent(actionContext)) {
      ServerPlayer initiator = actionContext.initiator();
      if (messageActionData.recipientScope() == MessageRecipientScope.OWNER
          && !isSamePlayer(initiator, resolveOwner(easyNPC))) {
        return List.of();
      }

      return canReceive(npcContext, initiator) ? List.of(initiator) : List.of();
    }

    Collection<ServerPlayer> candidates =
        switch (messageActionData.recipientScope()) {
          case INITIATOR -> toCollection(actionContext.initiator());
          case OWNER -> toCollection(resolveOwner(easyNPC));
          case NEARBY -> getServerPlayers(npcContext);
        };

    List<ServerPlayer> receivers = new ArrayList<>();
    for (ServerPlayer candidate : candidates) {
      if (canReceive(npcContext, candidate)) {
        receivers.add(candidate);
      }
    }
    return receivers;
  }

  /** Unlike the other outputs, a system message is not limited to the range around the NPC. */
  private static ServerPlayer resolveSystemMessageReceiver(
      EasyNPC<?> easyNPC, MessageActionData messageActionData, ActionContext actionContext) {
    if (isDistanceEvent(actionContext)) {
      ServerPlayer initiator = actionContext.initiator();
      if (messageActionData.recipientScope() == MessageRecipientScope.OWNER
          && !isSamePlayer(initiator, resolveOwner(easyNPC))) {
        return null;
      }

      return initiator;
    }

    if (messageActionData.recipientScope() == MessageRecipientScope.OWNER) {
      return resolveOwner(easyNPC);
    }

    return actionContext.initiator();
  }

  private static boolean isDistanceEvent(ActionContext actionContext) {
    return actionContext != null
        && actionContext.eventType() != null
        && actionContext.eventType().isDistanceEvent();
  }

  private static boolean isSamePlayer(ServerPlayer firstPlayer, ServerPlayer secondPlayer) {
    return firstPlayer != null
        && secondPlayer != null
        && firstPlayer.getUUID().equals(secondPlayer.getUUID());
  }

  private static ServerPlayer resolveOwner(EasyNPC<?> easyNPC) {
    OwnerDataCapable<?> ownerData = easyNPC.getEasyNPCOwnerData();
    if (ownerData == null || !ownerData.hasNPCOwner()) {
      return null;
    }

    return ownerData.getOwner() instanceof ServerPlayer serverPlayer ? serverPlayer : null;
  }

  private static Collection<ServerPlayer> toCollection(ServerPlayer serverPlayer) {
    return serverPlayer != null ? List.of(serverPlayer) : List.of();
  }

  private static Collection<ServerPlayer> getServerPlayers(LivingEntity npcContext) {
    List<ServerPlayer> serverPlayers = new ArrayList<>();
    for (Player player : npcContext.level().players()) {
      if (player instanceof ServerPlayer serverPlayer) {
        serverPlayers.add(serverPlayer);
      }
    }
    return serverPlayers;
  }

  private static boolean canReceive(LivingEntity npcContext, ServerPlayer receiver) {
    return receiver != null
        && EntitySelector.NO_SPECTATORS.test(receiver)
        && npcContext.closerThan(receiver, MESSAGE_RANGE)
        && !npcContext.isInvisibleTo(receiver);
  }

  private static Component parseMessage(
      ActionDataEntry actionDataEntry, ServerPlayer serverPlayer, LivingEntity npcContext) {
    String text = actionDataEntry.messageActionData().selectText();
    if (text == null || text.trim().isEmpty()) {
      return null;
    }

    if (TextUtils.isTranslationKey(text)) {
      return TextComponent.getTextComponentRaw(text, true);
    }

    text = ActionUtils.parseMacros(text, npcContext, serverPlayer);
    text = TextFormattingCodes.parseTextLineBreaks(text);
    text = TextFormattingCodes.parseTextFormattingCodes(text);

    return TextComponent.getText(TextUtils.limitString(text, MAX_MESSAGE_LENGTH));
  }

  private static Component resolveSenderName(
      MessageActionData messageActionData, LivingEntity npcContext) {
    if (!messageActionData.hasSenderName()) {
      return npcContext.getName();
    }

    String senderName = ChatFormatting.stripFormatting(messageActionData.senderName());
    if (senderName == null || senderName.isBlank()) {
      return npcContext.getName();
    }

    senderName = TextUtils.limitString(senderName, MessageActionData.MAX_SENDER_NAME_LENGTH);
    if (isKnownPlayerName(npcContext.level().getServer(), senderName)) {
      log.warn(
          "Refusing sender name '{}' of NPC {}, because it belongs to a player.",
          senderName,
          npcContext.getUUID());
      return npcContext.getName();
    }

    return TextComponent.getText(senderName);
  }

  private static boolean isKnownPlayerName(MinecraftServer minecraftServer, String senderName) {
    if (minecraftServer == null || !NameValidator.isValidPlayerName(senderName)) {
      return false;
    }

    if (minecraftServer.getPlayerList().getPlayerByName(senderName) != null) {
      return true;
    }

    // A profile cache miss ends in a blocking request against the Mojang API, so each name is only
    // looked up once per session.
    return knownPlayerNames.computeIfAbsent(
        senderName.toLowerCase(Locale.ROOT),
        name ->
            minecraftServer.getProfileCache() != null
                && minecraftServer.getProfileCache().get(name).isPresent());
  }
}
