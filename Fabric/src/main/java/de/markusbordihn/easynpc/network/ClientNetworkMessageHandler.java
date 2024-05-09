/*
 * Copyright 2023 Markus Bordihn
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software
 * and associated documentation files (the "Software"), to deal in the Software without restriction,
 * including without limitation the rights to use, copy, modify, merge, publish, distribute,
 * sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies or
 * substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING
 * BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
 * DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package de.markusbordihn.easynpc.network;

import de.markusbordihn.easynpc.data.cache.CacheType;
import de.markusbordihn.easynpc.entity.LivingEntityManager;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.PresetData;
import de.markusbordihn.easynpc.network.message.CacheDataSyncMessage;
import de.markusbordihn.easynpc.network.message.PresetExportClientMessage;
import java.util.UUID;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.server.level.ServerPlayer;

public class ClientNetworkMessageHandler implements ClientNetworkMessageHandlerInterface {

  @Override
  public void exportClientPreset(UUID uuid, String name, ServerPlayer serverPlayer) {
    if (name != null && !name.isEmpty() && NetworkMessage.checkAccess(uuid, serverPlayer)) {
      EasyNPC<?> easyNPC = LivingEntityManager.getEasyNPCEntityByUUID(uuid, serverPlayer);
      PresetData<?> presetData = easyNPC.getEasyNPCPresetData();
      CompoundTag compoundTag = presetData.exportPresetData();
      log.info(
          "Exporting preset for {} to {}",
          easyNPC.getEntity().getName().getString(),
          serverPlayer.getName().getString());
      NetworkHandler.sendToPlayer(
          serverPlayer,
          PresetExportClientMessage.MESSAGE_ID,
          new PresetExportClientMessage(
                  uuid,
                  easyNPC.getEntity().getName().getString(),
                  easyNPC.getEasyNPCSkinData().getSkinModel(),
                  name,
                  compoundTag)
              .encode());
    }
  }

  @Override
  public void syncCacheData(
      UUID uuid, ServerPlayer serverPlayer, CacheType cacheType, CompoundTag data) {
    if (uuid != null && serverPlayer != null && cacheType != null && data != null) {
      NetworkHandler.sendToPlayer(
          serverPlayer,
          CacheDataSyncMessage.MESSAGE_ID,
          new CacheDataSyncMessage(uuid, cacheType, data).encode());
    }
  }
}
