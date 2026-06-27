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

package de.markusbordihn.easynpc.configui.network.message.server;

import static org.junit.jupiter.api.Assertions.*;

import de.markusbordihn.easynpc.data.action.ActionDataEntry;
import de.markusbordihn.easynpc.data.action.ActionDataSet;
import de.markusbordihn.easynpc.data.action.ActionDataType;
import de.markusbordihn.easynpc.data.dialog.DialogButtonEntry;
import de.markusbordihn.easynpc.data.dialog.DialogDataEntry;
import de.markusbordihn.easynpc.security.CommandPermissionLevel;
import java.util.Set;
import org.junit.jupiter.api.Test;

class MessageSecurityTest {

  @Test
  void testKnownSecureRemoteUrls() {
    assertTrue(MessageSecurity.isKnownSecureRemoteUrl("https://www.minecraftskins.com/skin.png"));
    assertTrue(MessageSecurity.isKnownSecureRemoteUrl("https://minecraft.novaskin.me/skin.png"));
    assertTrue(MessageSecurity.isKnownSecureRemoteUrl("https://mcskins.top/skin.png"));
    assertTrue(MessageSecurity.isKnownSecureRemoteUrl("https://skinmc.net/skin.png"));
  }

  @Test
  void testUnknownRemoteUrlsAreRejected() {
    assertFalse(MessageSecurity.isKnownSecureRemoteUrl(null));
    assertFalse(MessageSecurity.isKnownSecureRemoteUrl(""));
    assertFalse(MessageSecurity.isKnownSecureRemoteUrl("http://www.minecraftskins.com/skin.png"));
    assertFalse(
        MessageSecurity.isKnownSecureRemoteUrl("https://www.minecraftskins.com.evil/skin.png"));
    assertFalse(MessageSecurity.isKnownSecureRemoteUrl("https://example.com/skin.png"));
  }

  @Test
  void testActionDataSetSanitizationClampsPermissionLevel() {
    ActionDataSet actionDataSet = new ActionDataSet();
    actionDataSet.add(
        new ActionDataEntry(
            ActionDataType.NONE, "", CommandPermissionLevel.OWNERS.minecraftLevel()));

    ActionDataSet sanitized =
        MessageSecurity.sanitizeActionDataSet(
            actionDataSet, null, null, CommandPermissionLevel.MODERATORS);

    assertNotNull(sanitized);
    assertEquals(1, sanitized.size());
    assertEquals(
        CommandPermissionLevel.MODERATORS,
        sanitized.getEntries().iterator().next().commandPermissionLevel());
  }

  @Test
  void testDialogSanitizationClampsButtonActionPermissionLevel() {
    ActionDataSet actionDataSet = new ActionDataSet();
    actionDataSet.add(
        new ActionDataEntry(
            ActionDataType.NONE, "", CommandPermissionLevel.ADMINS.minecraftLevel()));
    DialogButtonEntry button = new DialogButtonEntry("start", "Start", actionDataSet);
    DialogDataEntry dialogDataEntry =
        new DialogDataEntry("intro", "Intro", "Hello", Set.of(button));

    DialogDataEntry sanitized =
        MessageSecurity.sanitizeDialogDataEntry(
            dialogDataEntry, null, null, CommandPermissionLevel.ALL);

    assertNotNull(sanitized);
    ActionDataEntry sanitizedAction =
        sanitized
            .getDialogButtons()
            .iterator()
            .next()
            .actionDataSet()
            .getEntries()
            .iterator()
            .next();
    assertEquals(CommandPermissionLevel.ALL, sanitizedAction.commandPermissionLevel());
  }
}
