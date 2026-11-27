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

import de.markusbordihn.easynpc.data.preset.PresetMetadata;
import de.markusbordihn.easynpc.data.skin.SkinModel;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.SkinDataCapable;
import de.markusbordihn.easynpc.handler.PresetFeedback;
import de.markusbordihn.easynpc.handler.PresetHandler;
import de.markusbordihn.easynpc.io.PresetFileHandler;
import de.markusbordihn.easynpc.network.message.AccessCheckedMessageRecord;
import de.markusbordihn.easynpc.security.NpcFeature;
import de.markusbordihn.easynpc.security.PresetExportRateLimiter;
import java.io.File;
import java.util.function.BiFunction;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.server.level.ServerPlayer;

interface PresetExportMessageRecord extends AccessCheckedMessageRecord {

  String name();

  PresetMetadata metadata();

  default void exportToPresetFile(
      final EasyNPC<?> easyNPC,
      final ServerPlayer serverPlayer,
      final NpcFeature npcFeature,
      final String presetTypeName,
      final BiFunction<SkinModel, String, File> presetFileResolver) {
    if (!MessageSecurity.checkFeatureAccess(
        serverPlayer, easyNPC, npcFeature, presetTypeName + " export")) {
      return;
    }

    if (this.name() == null || this.name().isEmpty()) {
      log.warn("Export {} name is empty for {}", presetTypeName, easyNPC);
      return;
    }

    SkinDataCapable<?> skinData = easyNPC.getEasyNPCSkinData();
    if (skinData == null) {
      log.warn("Export {} skin data is empty for {}", presetTypeName, easyNPC);
      return;
    }

    SkinModel skinModel = skinData.getSkinModel();
    if (skinModel == null) {
      log.warn("Export {} skin model is empty for {}", presetTypeName, easyNPC);
      return;
    }

    CompoundTag exportData = PresetHandler.prepareClientExportData(easyNPC, this.metadata());
    if (exportData == null || exportData.isEmpty()) {
      log.warn("Export {} data is empty for {}", presetTypeName, easyNPC);
      return;
    }

    File presetFile = presetFileResolver.apply(skinModel, this.name());
    if (presetFile == null) {
      log.error(
          "Failed to get {} file for {} with name {}", presetTypeName, skinModel, this.name());
      return;
    }

    if (!PresetExportRateLimiter.checkAndRecord(serverPlayer)) {
      log.warn(
          "Rate-limited preset export by {}, see security.cfg key npcPresetExportRateLimit.",
          serverPlayer.getName());
      serverPlayer.sendSystemMessage(
          PresetFeedback.exportRateLimited(PresetExportRateLimiter.exportLimit()));
      return;
    }

    log.info(
        "Exporting EasyNPC {} with {} and skin {} to {} {}",
        this.name(),
        easyNPC,
        skinModel,
        presetTypeName,
        presetFile);
    if (!PresetFileHandler.saveNbt(presetFile, exportData)) {
      log.error(
          "Failed to export EasyNPC {} with {} and skin {} to {} {}",
          this.name(),
          easyNPC,
          skinModel,
          presetTypeName,
          presetFile);
      serverPlayer.sendSystemMessage(PresetFeedback.exportFailed(presetFile.getName()));
      return;
    }

    PresetFeedback.sendExportResult(serverPlayer, presetFile.getName(), this.uuid());
  }
}
