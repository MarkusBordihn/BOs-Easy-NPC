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

package de.markusbordihn.easynpc.handler;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.utils.TextUtils;
import de.markusbordihn.easynpc.utils.UUIDUtils;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;
import net.minecraft.network.chat.Component;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.level.ServerPlayer;

public class PresetFeedback {

  public static final int MAX_LISTED_NAMES = 5;
  private static final String PRESET_PREFIX = Constants.TEXT_PREFIX + "preset.";

  private PresetFeedback() {}

  public static void sendImportResult(
      ServerPlayer serverPlayer, PresetImportResult result, ResourceLocation presetLocation) {
    if (serverPlayer == null || result == null) {
      return;
    }

    serverPlayer.sendSystemMessage(importResult(result, presetLocation, serverPlayer));
  }

  public static Component importResult(
      PresetImportResult result, ResourceLocation presetLocation, ServerPlayer serverPlayer) {
    String presetName = presetName(presetLocation);
    String entityId = UUIDUtils.shortId(result.entityUUID());
    return switch (result.outcome()) {
      case CREATED ->
          Component.translatable(
              PRESET_PREFIX + "imported",
              presetName,
              entityId,
              TextUtils.formatTeleportPosition(result.position(), serverPlayer));
      case UPDATED_EXISTING ->
          Component.translatable(PRESET_PREFIX + "imported_updated", presetName, entityId);
      case REPLACED_EXISTING ->
          Component.translatable(PRESET_PREFIX + "imported_replaced", presetName, entityId);
      case FAILED -> importFailed(presetLocation);
    };
  }

  public static Component importFailed(ResourceLocation presetLocation) {
    return Component.translatable(PRESET_PREFIX + "import_failed", presetName(presetLocation));
  }

  public static void sendExportResult(ServerPlayer serverPlayer, String fileName, UUID entityUUID) {
    if (serverPlayer == null) {
      return;
    }

    serverPlayer.sendSystemMessage(exportResult(fileName, entityUUID));
    serverPlayer.sendSystemMessage(exportHint());
  }

  public static Component exportResult(String fileName, UUID entityUUID) {
    if (entityUUID == null) {
      return Component.translatable(PRESET_PREFIX + "exported_without_identity", fileName);
    }

    return Component.translatable(
        PRESET_PREFIX + "exported", fileName, UUIDUtils.shortId(entityUUID));
  }

  public static Component exportHint() {
    return Component.translatable(PRESET_PREFIX + "exported_hint");
  }

  public static Component exportFailed(String fileName) {
    return Component.translatable(PRESET_PREFIX + "export_failed", fileName);
  }

  public static Component spawnDenied() {
    return Component.translatable(PRESET_PREFIX + "spawn_denied");
  }

  public static Component restoreDenied() {
    return Component.translatable(PRESET_PREFIX + "restore_denied");
  }

  public static Component spawnRateLimited(int limit) {
    return Component.translatable(PRESET_PREFIX + "spawn_rate_limited", limit);
  }

  public static Component batchPatternInvalid(String pattern) {
    return Component.translatable(PRESET_PREFIX + "batch.pattern_invalid", pattern);
  }

  public static Component batchNoMatches(String pattern) {
    return Component.translatable(PRESET_PREFIX + "batch.no_matches", pattern);
  }

  public static Component batchTooMany(int matches, int limit) {
    return Component.translatable(PRESET_PREFIX + "batch.too_many", matches, limit);
  }

  public static Component batchCooldown(long remainingSeconds) {
    return Component.translatable(PRESET_PREFIX + "batch.cooldown", remainingSeconds);
  }

  public static List<Component> batchPreview(
      List<ResourceLocation> matches, int existingEntities, boolean keepIdentity) {
    String previewKey = keepIdentity ? "batch.preview" : "batch.preview_new";
    List<Component> messages = new ArrayList<>();
    messages.add(
        Component.translatable(
            PRESET_PREFIX + previewKey,
            matches.size(),
            matches.size() - existingEntities,
            existingEntities));
    messages.addAll(truncatedNames(matches));
    return messages;
  }

  public static Component batchResult(int created, int updated, int replaced, int failed) {
    return Component.translatable(
        PRESET_PREFIX + "batch.result", created, updated, replaced, failed);
  }

  private static List<Component> truncatedNames(List<ResourceLocation> matches) {
    List<Component> messages = new ArrayList<>();
    int shownNames = Math.min(matches.size(), MAX_LISTED_NAMES);
    for (int i = 0; i < shownNames; i++) {
      messages.add(
          Component.translatable(PRESET_PREFIX + "batch.entry", presetName(matches.get(i))));
    }

    int remainingNames = matches.size() - shownNames;
    if (remainingNames > 0) {
      messages.add(Component.translatable(PRESET_PREFIX + "batch.more", remainingNames));
    }

    return messages;
  }

  private static String presetName(ResourceLocation presetLocation) {
    if (presetLocation == null) {
      return "";
    }

    return presetLocation.getPath();
  }
}
