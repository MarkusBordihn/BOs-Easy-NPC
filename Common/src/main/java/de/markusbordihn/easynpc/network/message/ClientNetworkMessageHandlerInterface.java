/*
 * Copyright 2023 Markus Bordihn
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

package de.markusbordihn.easynpc.network.message;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.entity.LivingEntityManager;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.PresetData;
import de.markusbordihn.easynpc.network.NetworkHandlerManager;
import de.markusbordihn.easynpc.network.message.client.ExportClientPresetMessage;
import de.markusbordihn.easynpc.network.message.client.OpenMenuCallbackMessage;
import de.markusbordihn.easynpc.network.message.client.SyncDataMessage;
import java.util.UUID;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.server.level.ServerPlayer;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public interface ClientNetworkMessageHandlerInterface {

  Logger log = LogManager.getLogger(Constants.LOG_NAME);

  default void exportClientPreset(
      final UUID uuid, final String name, final ServerPlayer serverPlayer) {
    if (name == null || name.isEmpty() || !NetworkMessageRecord.checkAccess(uuid, serverPlayer)) {
      return;
    }

    EasyNPC<?> easyNPC = LivingEntityManager.getEasyNPCEntityByUUID(uuid, serverPlayer);
    PresetData<?> presetData = easyNPC.getEasyNPCPresetData();
    CompoundTag compoundTag = presetData.exportPresetData();
    log.info(
        "Exporting preset for {} to {}",
        easyNPC.getEntity().getName().getString(),
        serverPlayer.getName().getString());
    NetworkHandlerManager.sendToPlayer(
        new ExportClientPresetMessage(
            uuid,
            easyNPC.getEntity().getName().getString(),
            easyNPC.getEasyNPCSkinData().getSkinModel(),
            name,
            compoundTag),
        serverPlayer);
  }

  default void openMenu(UUID uuid, UUID menuId, ServerPlayer serverPlayer, CompoundTag data) {
    if (uuid != null && menuId != null && serverPlayer != null) {
      log.info("Open menu with UUID {}", uuid);
      NetworkHandlerManager.sendToPlayer(
          new OpenMenuCallbackMessage(uuid, menuId, data), serverPlayer);
    }
  }

  default void syncData(EasyNPC<?> easyNPC) {
    if (easyNPC != null) {
      log.debug("Sync {} data to all players.", easyNPC);
      NetworkHandlerManager.sendToAllPlayers(
          new SyncDataMessage(
              easyNPC.getUUID(),
              easyNPC.getEasyNPCDialogData() != null
                  ? easyNPC.getEasyNPCDialogData().getDialogDataSet()
                  : null));
    }
  }

  default void syncData(EasyNPC<?> easyNPC, ServerPlayer serverPlayer) {
    if (easyNPC != null && serverPlayer != null) {
      log.debug("Sync {} data to player {}", easyNPC, serverPlayer);
      NetworkHandlerManager.sendToPlayer(
          new SyncDataMessage(
              easyNPC.getUUID(),
              easyNPC.getEasyNPCDialogData() != null
                  ? easyNPC.getEasyNPCDialogData().getDialogDataSet()
                  : null),
          serverPlayer);
    }
  }
}
