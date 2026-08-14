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

package de.markusbordihn.easynpc.api.action;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.data.action.ActionContext;
import de.markusbordihn.easynpc.data.action.ActionDataEntry;
import de.markusbordihn.easynpc.data.action.ActionDataType;
import de.markusbordihn.easynpc.data.action.ActionEventType;
import de.markusbordihn.easynpc.data.action.MessageActionData;
import de.markusbordihn.easynpc.data.action.SoundActionData;
import de.markusbordihn.easynpc.data.action.SpeechBubbleManager;
import de.markusbordihn.easynpc.data.state.StateEntry;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.ActionEventDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.DialogDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.StateDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.TradingDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.handlers.ActionHandler;
import de.markusbordihn.easynpc.entity.easynpc.handlers.action.executor.DialogActionExecutor;
import de.markusbordihn.easynpc.entity.easynpc.handlers.action.executor.MessageActionExecutor;
import de.markusbordihn.easynpc.entity.easynpc.handlers.action.executor.ScoreboardActionExecutor;
import de.markusbordihn.easynpc.entity.easynpc.handlers.action.executor.SoundActionExecutor;
import de.markusbordihn.easynpc.network.components.TextComponent;
import de.markusbordihn.easynpc.utils.TextUtils;
import java.util.Collection;
import java.util.List;
import net.minecraft.network.chat.Component;
import net.minecraft.resources.Identifier;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.sounds.SoundSource;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

/**
 * Programmatic entry point for NPC actions. Methods use the same executors as preset actions and
 * retain sender name validation, dialog conditions and trading checks.
 *
 * <p>Command actions have no convenience method because their permission level belongs to the
 * {@link ActionDataEntry}. Use {@link #execute} for those actions.
 */
public class EasyNPCActionHandler {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private EasyNPCActionHandler() {}

  public static boolean say(EasyNPC<?> easyNPC, String text) {
    return say(easyNPC, toTextList(text));
  }

  public static boolean say(EasyNPC<?> easyNPC, List<String> texts) {
    return sendMessage(easyNPC, texts, MessageActionData.DEFAULT, ActionContext.EMPTY);
  }

  public static boolean sayTo(EasyNPC<?> easyNPC, ServerPlayer receiver, String text) {
    return sayTo(easyNPC, receiver, toTextList(text));
  }

  public static boolean sayTo(EasyNPC<?> easyNPC, ServerPlayer receiver, List<String> texts) {
    if (receiver == null) {
      log.error("Unable to let {} talk to a null player", easyNPC);
      return false;
    }

    return sendMessage(
        easyNPC,
        texts,
        MessageActionData.DEFAULT.withShowInNearbyChat(false).withShowAsSystemMessage(true),
        ActionContext.of(receiver));
  }

  public static boolean sayTo(EasyNPC<?> easyNPC, Collection<ServerPlayer> receivers, String text) {
    return sayTo(easyNPC, receivers, toTextList(text));
  }

  public static boolean sayTo(
      EasyNPC<?> easyNPC, Collection<ServerPlayer> receivers, List<String> texts) {
    if (receivers == null || receivers.isEmpty()) {
      log.error("Unable to let {} talk to an empty list of players", easyNPC);
      return false;
    }

    String selectedText = selectText(texts);
    if (selectedText == null) {
      log.error("Unable to let {} say an empty text list", easyNPC);
      return false;
    }

    boolean allDelivered = true;
    for (ServerPlayer receiver : receivers) {
      allDelivered &= sayTo(easyNPC, receiver, selectedText);
    }
    return allDelivered;
  }

  public static boolean showSpeechBubble(EasyNPC<?> easyNPC, String text) {
    return showSpeechBubble(easyNPC, toTextList(text));
  }

  public static boolean showSpeechBubble(EasyNPC<?> easyNPC, List<String> texts) {
    return showSpeechBubble(easyNPC, texts, SpeechBubbleManager.DEFAULT_DURATION_TICKS);
  }

  public static boolean showSpeechBubble(EasyNPC<?> easyNPC, String text, int durationTicks) {
    return showSpeechBubble(easyNPC, toTextList(text), durationTicks);
  }

  public static boolean showSpeechBubble(
      EasyNPC<?> easyNPC, List<String> texts, int durationTicks) {
    Component message = toMessage(easyNPC, selectText(texts));
    if (message == null) {
      return false;
    }

    MessageActionExecutor.send(
        easyNPC,
        message,
        MessageActionData.DEFAULT.withShowInNearbyChat(false).withShowAsSpeechBubble(true),
        ActionContext.EMPTY,
        durationTicks);
    return true;
  }

  public static boolean sendMessage(
      EasyNPC<?> easyNPC,
      String text,
      MessageActionData messageActionData,
      ActionContext actionContext) {
    return sendMessage(easyNPC, toTextList(text), messageActionData, actionContext);
  }

  public static boolean sendMessage(
      EasyNPC<?> easyNPC,
      List<String> texts,
      MessageActionData messageActionData,
      ActionContext actionContext) {
    Component message = toMessage(easyNPC, selectText(texts));
    if (message == null) {
      return false;
    }

    MessageActionExecutor.send(
        easyNPC,
        message,
        messageActionData,
        actionContext != null ? actionContext : ActionContext.EMPTY,
        SpeechBubbleManager.DEFAULT_DURATION_TICKS);
    return true;
  }

  public static boolean playSound(EasyNPC<?> easyNPC, String soundId) {
    return playSound(easyNPC, new SoundActionData(soundId));
  }

  public static boolean playSound(
      EasyNPC<?> easyNPC, String soundId, SoundSource soundSource, float volume, float pitch) {
    return playSound(easyNPC, new SoundActionData(soundId, soundSource, volume, pitch));
  }

  public static boolean playSound(EasyNPC<?> easyNPC, SoundActionData soundActionData) {
    if (!isUsable(easyNPC) || soundActionData == null || !soundActionData.hasSoundId()) {
      log.error("Unable to play sound {} for {}", soundActionData, easyNPC);
      return false;
    }

    return SoundActionExecutor.play(
        new ActionDataEntry(ActionDataType.SOUND).withSoundActionData(soundActionData), easyNPC);
  }

  public static boolean setState(
      EasyNPC<?> easyNPC, Identifier stateId, StateEntry stateEntry, ServerPlayer initiator) {
    if (!isUsable(easyNPC) || stateId == null || stateEntry == null) {
      log.error("Unable to set state {} of {} to {}", stateId, easyNPC, stateEntry);
      return false;
    }

    StateDataCapable<?> stateData = easyNPC.getEasyNPCStateData();
    if (stateData == null) {
      log.error("Unable to set state {}: {} has no state data", stateId, easyNPC);
      return false;
    }

    stateData.setState(stateId, stateEntry, ActionContext.of(initiator));
    return true;
  }

  public static StateEntry getState(EasyNPC<?> easyNPC, Identifier stateId) {
    if (easyNPC == null || stateId == null) {
      return null;
    }

    StateDataCapable<?> stateData = easyNPC.getEasyNPCStateData();
    return stateData != null ? stateData.getState(stateId) : null;
  }

  public static boolean openDialog(EasyNPC<?> easyNPC, ServerPlayer serverPlayer) {
    return openDialog(easyNPC, serverPlayer, null);
  }

  public static boolean openDialog(
      EasyNPC<?> easyNPC, ServerPlayer serverPlayer, String dialogLabel) {
    if (!isUsable(easyNPC) || serverPlayer == null) {
      log.error("Unable to open a dialog of {} for {}", easyNPC, serverPlayer);
      return false;
    }

    DialogDataCapable<?> dialogData = easyNPC.getEasyNPCDialogData();
    if (dialogData == null || !dialogData.hasDialog()) {
      log.error("Unable to open a dialog: {} has no dialog data", easyNPC);
      return false;
    }

    if (dialogLabel == null || dialogLabel.isEmpty()) {
      DialogActionExecutor.openDefaultDialog(
          new ActionDataEntry(ActionDataType.OPEN_DEFAULT_DIALOG), serverPlayer, dialogData);
      return true;
    }

    DialogActionExecutor.openNamedDialog(
        new ActionDataEntry(ActionDataType.OPEN_NAMED_DIALOG, dialogLabel),
        serverPlayer,
        dialogData);
    return true;
  }

  public static boolean openTradingScreen(EasyNPC<?> easyNPC, ServerPlayer serverPlayer) {
    if (!isUsable(easyNPC) || serverPlayer == null) {
      log.error("Unable to open the trading screen of {} for {}", easyNPC, serverPlayer);
      return false;
    }

    TradingDataCapable<?> tradingData = easyNPC.getEasyNPCTradingData();
    if (tradingData == null || !tradingData.hasTradingData()) {
      log.error("Unable to open the trading screen: {} has no trading data", easyNPC);
      return false;
    }

    tradingData.openTradingScreen(serverPlayer);
    return true;
  }

  public static boolean updateScoreboard(
      EasyNPC<?> easyNPC, ServerPlayer serverPlayer, String command) {
    if (!isUsable(easyNPC) || serverPlayer == null || command == null || command.isEmpty()) {
      log.error("Unable to update the scoreboard of {} with {}", serverPlayer, command);
      return false;
    }

    ScoreboardActionExecutor.execute(
        new ActionDataEntry(ActionDataType.SCOREBOARD, command),
        serverPlayer,
        easyNPC.getLivingEntity());
    return true;
  }

  /** Runs the entry as it is, without checking the conditions attached to it. */
  public static boolean execute(
      EasyNPC<?> easyNPC, ActionDataEntry actionDataEntry, ActionContext actionContext) {
    if (!isUsable(easyNPC) || actionDataEntry == null) {
      log.error("Unable to execute action {} for {}", actionDataEntry, easyNPC);
      return false;
    }

    ActionHandler<?> actionHandler = easyNPC.getEasyNPCActionHandler();
    if (actionHandler == null) {
      log.error("Unable to execute action {}: {} has no action handler", actionDataEntry, easyNPC);
      return false;
    }

    actionHandler.executeAction(
        actionDataEntry, actionContext != null ? actionContext : ActionContext.EMPTY);
    return true;
  }

  /** Runs the stored action set of the event, including the conditions attached to its entries. */
  public static boolean trigger(
      EasyNPC<?> easyNPC, ActionEventType actionEventType, ActionContext actionContext) {
    if (!isUsable(easyNPC) || actionEventType == null) {
      log.error("Unable to trigger event {} for {}", actionEventType, easyNPC);
      return false;
    }

    ActionEventDataCapable<?> actionEventData = easyNPC.getEasyNPCActionEventData();
    if (actionEventData == null) {
      log.error("Unable to trigger event {}: {} has no action data", actionEventType, easyNPC);
      return false;
    }

    actionEventData.handleActionEvent(
        actionEventType, actionContext != null ? actionContext : ActionContext.EMPTY);
    return true;
  }

  private static List<String> toTextList(String text) {
    return text != null ? List.of(text) : List.of();
  }

  private static String selectText(Collection<String> texts) {
    return MessageActionData.DEFAULT.withTexts(texts).selectText();
  }

  private static Component toMessage(EasyNPC<?> easyNPC, String text) {
    if (!isUsable(easyNPC)) {
      log.error("Unable to let {} say {}", easyNPC, text);
      return null;
    }

    if (text == null || text.isBlank()) {
      log.error("Unable to let {} say an empty text", easyNPC);
      return null;
    }

    if (TextUtils.isTranslationKey(text)) {
      return TextComponent.getTextComponentRaw(text, true);
    }

    return TextComponent.getText(text);
  }

  private static boolean isUsable(EasyNPC<?> easyNPC) {
    return easyNPC != null && !easyNPC.isClientSideInstance();
  }
}
