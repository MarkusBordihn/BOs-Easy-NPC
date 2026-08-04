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

package de.markusbordihn.easynpc.data.action;

import de.markusbordihn.easynpc.utils.TextUtils;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.concurrent.ThreadLocalRandom;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.ListTag;
import net.minecraft.nbt.StringTag;
import net.minecraft.nbt.Tag;

public record MessageActionData(
    boolean showInNearbyChat,
    boolean showAsSystemMessage,
    boolean showAsSpeechBubble,
    String senderName,
    MessageRecipientScope recipientScope,
    List<String> texts) {

  public static final MessageActionData DEFAULT = new MessageActionData(true, false, false, "");
  public static final int MAX_TEXTS = 6;
  public static final int MAX_SENDER_NAME_LENGTH = 32;

  private static final String DATA_TARGETS_TAG = "T";
  private static final String DATA_SENDER_NAME_TAG = "Sender";
  private static final String DATA_RECIPIENT_SCOPE_TAG = "R";
  private static final String DATA_TEXTS_TAG = "Texts";
  private static final int TARGET_NEARBY_CHAT_BIT = 1;
  private static final int TARGET_SYSTEM_MESSAGE_BIT = 2;
  private static final int TARGET_SPEECH_BUBBLE_BIT = 4;

  public MessageActionData {
    senderName =
        senderName == null || senderName.isBlank() || TextUtils.isTranslationKey(senderName)
            ? ""
            : TextUtils.limitString(senderName, MAX_SENDER_NAME_LENGTH);
    recipientScope = recipientScope == null ? MessageRecipientScope.NEARBY : recipientScope;
    texts = normalizeTexts(texts);
  }

  public MessageActionData(
      boolean showInNearbyChat,
      boolean showAsSystemMessage,
      boolean showAsSpeechBubble,
      String senderName) {
    this(
        showInNearbyChat,
        showAsSystemMessage,
        showAsSpeechBubble,
        senderName,
        MessageRecipientScope.NEARBY,
        List.of());
  }

  public MessageActionData(
      boolean showInNearbyChat,
      boolean showAsSystemMessage,
      boolean showAsSpeechBubble,
      String senderName,
      MessageRecipientScope recipientScope) {
    this(
        showInNearbyChat,
        showAsSystemMessage,
        showAsSpeechBubble,
        senderName,
        recipientScope,
        List.of());
  }

  public static MessageActionData fromTag(CompoundTag compoundTag) {
    if (compoundTag == null || compoundTag.isEmpty()) {
      return DEFAULT;
    }

    byte targets = compoundTag.getByte(DATA_TARGETS_TAG);
    return new MessageActionData(
        (targets & TARGET_NEARBY_CHAT_BIT) != 0,
        (targets & TARGET_SYSTEM_MESSAGE_BIT) != 0,
        (targets & TARGET_SPEECH_BUBBLE_BIT) != 0,
        compoundTag.getString(DATA_SENDER_NAME_TAG),
        MessageRecipientScope.get(compoundTag.getString(DATA_RECIPIENT_SCOPE_TAG)),
        loadTexts(compoundTag));
  }

  private static List<String> loadTexts(CompoundTag compoundTag) {
    ListTag textsTag = compoundTag.getList(DATA_TEXTS_TAG, Tag.TAG_STRING);
    List<String> texts = new ArrayList<>();
    for (int index = 0; index < textsTag.size(); index++) {
      texts.add(textsTag.getString(index));
    }
    return texts;
  }

  private static List<String> normalizeTexts(Collection<String> texts) {
    if (texts == null || texts.isEmpty()) {
      return List.of();
    }

    return texts.stream()
        .filter(text -> text != null && !text.trim().isEmpty())
        .map(String::trim)
        .limit(MAX_TEXTS)
        .toList();
  }

  public CompoundTag createTag() {
    CompoundTag compoundTag = new CompoundTag();
    compoundTag.putByte(DATA_TARGETS_TAG, (byte) this.targetBits());
    if (!this.senderName.isEmpty()) {
      compoundTag.putString(DATA_SENDER_NAME_TAG, this.senderName);
    }
    if (this.recipientScope != MessageRecipientScope.NEARBY) {
      compoundTag.putString(DATA_RECIPIENT_SCOPE_TAG, this.recipientScope.name());
    }
    if (!this.texts.isEmpty()) {
      ListTag textsTag = new ListTag();
      for (String text : this.texts) {
        textsTag.add(StringTag.valueOf(text));
      }
      compoundTag.put(DATA_TEXTS_TAG, textsTag);
    }

    return compoundTag;
  }

  public String selectText() {
    if (this.texts.isEmpty()) {
      return null;
    }

    return this.texts.get(ThreadLocalRandom.current().nextInt(this.texts.size()));
  }

  public boolean hasAnyTarget() {
    return this.showInNearbyChat || this.showAsSystemMessage || this.showAsSpeechBubble;
  }

  public boolean hasSenderName() {
    return !this.senderName.isEmpty();
  }

  public boolean hasTexts() {
    return !this.texts.isEmpty();
  }

  public MessageActionData withShowInNearbyChat(boolean showInNearbyChat) {
    return new MessageActionData(
        showInNearbyChat,
        this.showAsSystemMessage,
        this.showAsSpeechBubble,
        this.senderName,
        this.recipientScope,
        this.texts);
  }

  public MessageActionData withShowAsSystemMessage(boolean showAsSystemMessage) {
    return new MessageActionData(
        this.showInNearbyChat,
        showAsSystemMessage,
        this.showAsSpeechBubble,
        this.senderName,
        this.recipientScope,
        this.texts);
  }

  public MessageActionData withShowAsSpeechBubble(boolean showAsSpeechBubble) {
    return new MessageActionData(
        this.showInNearbyChat,
        this.showAsSystemMessage,
        showAsSpeechBubble,
        this.senderName,
        this.recipientScope,
        this.texts);
  }

  public MessageActionData withSenderName(String senderName) {
    return new MessageActionData(
        this.showInNearbyChat,
        this.showAsSystemMessage,
        this.showAsSpeechBubble,
        senderName,
        this.recipientScope,
        this.texts);
  }

  public MessageActionData withRecipientScope(MessageRecipientScope recipientScope) {
    return new MessageActionData(
        this.showInNearbyChat,
        this.showAsSystemMessage,
        this.showAsSpeechBubble,
        this.senderName,
        recipientScope,
        this.texts);
  }

  public MessageActionData withTexts(Collection<String> texts) {
    return new MessageActionData(
        this.showInNearbyChat,
        this.showAsSystemMessage,
        this.showAsSpeechBubble,
        this.senderName,
        this.recipientScope,
        texts != null ? new ArrayList<>(texts) : List.of());
  }

  public MessageActionData withText(String text) {
    return this.withTexts(text != null ? List.of(text) : List.of());
  }

  private int targetBits() {
    return (this.showInNearbyChat ? TARGET_NEARBY_CHAT_BIT : 0)
        | (this.showAsSystemMessage ? TARGET_SYSTEM_MESSAGE_BIT : 0)
        | (this.showAsSpeechBubble ? TARGET_SPEECH_BUBBLE_BIT : 0);
  }
}
