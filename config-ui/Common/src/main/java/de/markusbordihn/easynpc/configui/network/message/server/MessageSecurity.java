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

import de.markusbordihn.easynpc.data.action.ActionDataEntry;
import de.markusbordihn.easynpc.data.action.ActionDataSet;
import de.markusbordihn.easynpc.data.dialog.DialogButtonEntry;
import de.markusbordihn.easynpc.data.dialog.DialogDataEntry;
import de.markusbordihn.easynpc.data.dialog.DialogDataSet;
import de.markusbordihn.easynpc.data.preset.PresetType;
import de.markusbordihn.easynpc.data.screen.ScreenData;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.menu.dialog.DialogMenu;
import de.markusbordihn.easynpc.security.CommandPermissionLevel;
import de.markusbordihn.easynpc.security.FeatureSecurity;
import de.markusbordihn.easynpc.security.NpcFeature;
import java.util.LinkedHashSet;
import java.util.Objects;
import java.util.Set;
import java.util.UUID;
import net.minecraft.server.level.ServerPlayer;

class MessageSecurity {

  private static final String[] KNOWN_SECURE_REMOTE_URLS = {
    "https://www.minecraftskins.com/",
    "https://minecraft.novaskin.me/",
    "https://mcskins.top/",
    "https://skinmc.net/"
  };

  private MessageSecurity() {}

  static boolean checkDialogSession(final UUID uuid, final ServerPlayer serverPlayer) {
    return checkDialogSession(uuid, null, serverPlayer);
  }

  static boolean checkDialogSession(
      final UUID uuid, final UUID dialogId, final ServerPlayer serverPlayer) {
    if (uuid == null || serverPlayer == null) {
      return false;
    }

    if (!(serverPlayer.containerMenu instanceof DialogMenu dialogMenu)) {
      return false;
    }

    ScreenData screenData = dialogMenu.getScreenData();
    if (screenData == null || !uuid.equals(screenData.uuid())) {
      return false;
    }

    return dialogId == null || Objects.equals(dialogId, screenData.dialogId());
  }

  static ActionDataSet sanitizeActionDataSet(
      ActionDataSet dataSet,
      EasyNPC<?> easyNPC,
      ServerPlayer serverPlayer,
      CommandPermissionLevel ceiling) {
    if (dataSet == null) {
      return null;
    }

    ActionDataSet sanitized = new ActionDataSet();
    for (ActionDataEntry entry : dataSet.getEntries()) {
      NpcFeature feature = FeatureSecurity.getFeature(entry.actionDataType());
      if (feature != null
          && !FeatureSecurity.checkFeatureAccess(serverPlayer, easyNPC, feature).allowed()) {
        return null;
      }

      CommandPermissionLevel clamped =
          CommandPermissionLevel.min(entry.commandPermissionLevel(), ceiling);
      if (clamped != entry.commandPermissionLevel()) {
        sanitized.add(
            new ActionDataEntry(
                entry.actionDataType(),
                entry.conditionDataSet(),
                entry.command(),
                entry.targetUUID(),
                entry.blockPos(),
                entry.executeAsUser(),
                entry.enableDebug(),
                clamped.minecraftLevel()));
      } else {
        sanitized.add(entry);
      }
    }

    return sanitized;
  }

  static DialogButtonEntry sanitizeDialogButtonEntry(
      DialogButtonEntry dialogButtonEntry,
      EasyNPC<?> easyNPC,
      ServerPlayer serverPlayer,
      CommandPermissionLevel ceiling) {
    if (dialogButtonEntry == null) {
      return null;
    }

    ActionDataSet sanitizedActionDataSet =
        sanitizeActionDataSet(dialogButtonEntry.actionDataSet(), easyNPC, serverPlayer, ceiling);
    return sanitizedActionDataSet != null
        ? dialogButtonEntry.withActionDataSet(sanitizedActionDataSet)
        : null;
  }

  static DialogDataEntry sanitizeDialogDataEntry(
      DialogDataEntry dialogDataEntry,
      EasyNPC<?> easyNPC,
      ServerPlayer serverPlayer,
      CommandPermissionLevel ceiling) {
    if (dialogDataEntry == null) {
      return null;
    }

    Set<DialogButtonEntry> sanitizedButtons = new LinkedHashSet<>();
    for (DialogButtonEntry dialogButtonEntry : dialogDataEntry.getDialogButtons()) {
      DialogButtonEntry sanitizedButton =
          sanitizeDialogButtonEntry(dialogButtonEntry, easyNPC, serverPlayer, ceiling);
      if (sanitizedButton == null) {
        return null;
      }

      sanitizedButtons.add(sanitizedButton);
    }

    dialogDataEntry.setDialogButtons(sanitizedButtons);
    return dialogDataEntry;
  }

  static DialogDataSet sanitizeDialogDataSet(
      DialogDataSet dialogDataSet,
      EasyNPC<?> easyNPC,
      ServerPlayer serverPlayer,
      CommandPermissionLevel ceiling) {
    if (dialogDataSet == null) {
      return null;
    }

    for (DialogDataEntry dialogDataEntry : dialogDataSet.getDialogsByLabel()) {
      if (sanitizeDialogDataEntry(dialogDataEntry, easyNPC, serverPlayer, ceiling) == null) {
        return null;
      }
    }

    return dialogDataSet;
  }

  static boolean checkPresetFeatureAccess(
      ServerPlayer serverPlayer, EasyNPC<?> easyNPC, PresetType presetType) {
    NpcFeature feature = getPresetFeature(presetType);
    return feature == null
        || FeatureSecurity.checkFeatureAccess(serverPlayer, easyNPC, feature).allowed();
  }

  static boolean isKnownSecureRemoteUrl(String url) {
    if (url == null || !url.startsWith("https://")) {
      return false;
    }

    for (String secureUrl : KNOWN_SECURE_REMOTE_URLS) {
      if (url.startsWith(secureUrl)) {
        return true;
      }
    }

    return false;
  }

  private static NpcFeature getPresetFeature(PresetType presetType) {
    if (presetType == null) {
      return null;
    }

    return switch (presetType) {
      case CUSTOM -> NpcFeature.CUSTOM_PRESET;
      case DEFAULT -> NpcFeature.DEFAULT_PRESET_IMPORT;
      case LOCAL -> NpcFeature.LOCAL_PRESET;
      case WORLD -> NpcFeature.WORLD_PRESET;
      case DATA -> null;
    };
  }
}
