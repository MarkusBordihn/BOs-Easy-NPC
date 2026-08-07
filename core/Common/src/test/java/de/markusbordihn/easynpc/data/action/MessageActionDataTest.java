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

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.List;
import net.minecraft.nbt.CompoundTag;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

class MessageActionDataTest {

  @Test
  @DisplayName("Every target combination survives a tag round trip")
  void testTargetRoundTrip() {
    for (int targets = 0; targets < 8; targets++) {
      MessageActionData messageActionData =
          new MessageActionData(
              (targets & 1) != 0, (targets & 2) != 0, (targets & 4) != 0, "Storyteller");

      assertEquals(
          messageActionData,
          MessageActionData.fromTag(messageActionData.createTag()),
          "Targets " + targets);
    }
  }

  @Test
  @DisplayName("A missing or empty tag falls back to the default")
  void testFromEmptyTag() {
    assertEquals(MessageActionData.DEFAULT, MessageActionData.fromTag(null));
    assertEquals(MessageActionData.DEFAULT, MessageActionData.fromTag(new CompoundTag()));
  }

  @Test
  @DisplayName("The default shows the message in the nearby chat only")
  void testDefaultTargets() {
    assertTrue(MessageActionData.DEFAULT.showInNearbyChat());
    assertFalse(MessageActionData.DEFAULT.showAsSystemMessage());
    assertFalse(MessageActionData.DEFAULT.showAsSpeechBubble());
    assertFalse(MessageActionData.DEFAULT.hasSenderName());
    assertFalse(MessageActionData.DEFAULT.hasTexts());
    assertTrue(MessageActionData.DEFAULT.hasAnyTarget());
  }

  @Test
  @DisplayName("An entry without any target is detected")
  void testWithoutAnyTarget() {
    assertFalse(new MessageActionData(false, false, false, "").hasAnyTarget());
  }

  @Test
  @DisplayName("An empty sender name is not stored")
  void testEmptySenderNameIsNotStored() {
    CompoundTag compoundTag = MessageActionData.DEFAULT.createTag();

    assertFalse(compoundTag.contains("Sender"));
  }

  @Test
  @DisplayName("A null sender name becomes an empty one")
  void testNullSenderName() {
    assertEquals("", new MessageActionData(true, false, false, null).senderName());
  }

  @Test
  @DisplayName("A translation key cannot be used as sender name")
  void testTranslationKeySenderName() {
    MessageActionData messageActionData =
        new MessageActionData(true, false, false, "entity.minecraft.player");

    assertFalse(messageActionData.hasSenderName());
    assertFalse(messageActionData.createTag().contains("Sender"));
  }

  @Test
  @DisplayName("A too long sender name is limited")
  void testSenderNameLimit() {
    String senderName = "A".repeat(MessageActionData.MAX_SENDER_NAME_LENGTH + 10);

    assertEquals(
        MessageActionData.MAX_SENDER_NAME_LENGTH + 1,
        new MessageActionData(true, false, false, senderName).senderName().length());
  }

  @Test
  @DisplayName("Every recipient scope survives a tag round trip")
  void testRecipientScopeRoundTrip() {
    for (MessageRecipientScope recipientScope : MessageRecipientScope.values()) {
      MessageActionData messageActionData =
          MessageActionData.DEFAULT.withRecipientScope(recipientScope);

      assertEquals(
          messageActionData,
          MessageActionData.fromTag(messageActionData.createTag()),
          "Scope " + recipientScope);
    }
  }

  @Test
  @DisplayName("The default recipient scope is omitted")
  void testDefaultRecipientScopeIsNotStored() {
    assertEquals(MessageRecipientScope.NEARBY, MessageActionData.DEFAULT.recipientScope());
    assertFalse(MessageActionData.DEFAULT.createTag().contains("R"));
    assertTrue(
        MessageActionData.DEFAULT
            .withRecipientScope(MessageRecipientScope.OWNER)
            .createTag()
            .contains("R"));
  }

  @Test
  @DisplayName("An unknown scope falls back to the nearby players")
  void testUnknownRecipientScope() {
    CompoundTag compoundTag = MessageActionData.DEFAULT.createTag();
    compoundTag.putString("R", "SOMEWHERE_ELSE");

    assertEquals(
        MessageRecipientScope.NEARBY, MessageActionData.fromTag(compoundTag).recipientScope());
  }

  @Test
  @DisplayName("Message texts survive a tag round trip")
  void testTextRoundTrip() {
    MessageActionData messageActionData =
        MessageActionData.DEFAULT.withTexts(List.of("Hello", "Welcome back"));

    assertEquals(messageActionData, MessageActionData.fromTag(messageActionData.createTag()));
  }

  @Test
  @DisplayName("Empty and excessive message texts are removed")
  void testTextNormalization() {
    MessageActionData messageActionData =
        MessageActionData.DEFAULT.withTexts(
            List.of(" One ", "", "Two", "Three", "Four", "Five", "Six"));

    assertEquals(List.of("One", "Two", "Three", "Four", "Five", "Six"), messageActionData.texts());
  }

  @Test
  @DisplayName("A single message text remains unchanged")
  void testSingleTextSelection() {
    assertEquals("Only text", MessageActionData.DEFAULT.withText("Only text").selectText());
  }

  @Test
  @DisplayName("No message can be selected from an empty text list")
  void testEmptyTextSelection() {
    assertNull(MessageActionData.DEFAULT.selectText());
  }
}
