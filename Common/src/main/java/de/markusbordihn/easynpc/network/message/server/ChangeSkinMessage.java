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

package de.markusbordihn.easynpc.network.message.server;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.data.skin.SkinType;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.handler.SkinHandler;
import de.markusbordihn.easynpc.network.message.NetworkMessageRecord;
import de.markusbordihn.easynpc.utils.PlayersUtils;
import java.util.UUID;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.level.ServerPlayer;

public record ChangeSkinMessage(
    UUID uuid,
    String skinName,
    String skinURL,
    UUID skinUUID,
    SkinType skinType,
    String skinVariant)
    implements NetworkMessageRecord {

  public static final ResourceLocation MESSAGE_ID =
      new ResourceLocation(Constants.MOD_ID, "change_skin");

  public static ChangeSkinMessage create(final FriendlyByteBuf buffer) {
    return new ChangeSkinMessage(
        buffer.readUUID(),
        buffer.readUtf(),
        buffer.readUtf(),
        buffer.readUUID(),
        buffer.readEnum(SkinType.class),
        buffer.readUtf());
  }

  @Override
  public void write(final FriendlyByteBuf buffer) {
    buffer.writeUUID(this.uuid);
    buffer.writeUtf(this.skinName);
    buffer.writeUtf(this.skinURL);
    buffer.writeUUID(this.skinUUID);
    buffer.writeEnum(this.skinType);
    buffer.writeUtf(this.skinVariant);
  }

  @Override
  public ResourceLocation id() {
    return MESSAGE_ID;
  }

  @Override
  public void handleServer(final ServerPlayer serverPlayer) {
    EasyNPC<?> easyNPC = getEasyNPCAndCheckAccess(this.uuid, serverPlayer);
    if (easyNPC == null
        || this.skinName == null
        || this.skinType == null
        || easyNPC.getEasyNPCSkinData() == null) {
      log.error("Skin validation failed for {} from {}", easyNPC, serverPlayer);
      return;
    }

    boolean successfullyChanged =
        switch (this.skinType) {
          case NONE -> SkinHandler.setNoneSkin(easyNPC);
          case CUSTOM -> SkinHandler.setCustomSkin(easyNPC, this.skinUUID);
          case DEFAULT -> SkinHandler.setDefaultSkin(easyNPC, this.skinVariant);
          case PLAYER_SKIN -> {
            UUID userUUID = this.skinUUID;
            if (userUUID == null || Constants.BLANK_UUID.equals(this.skinUUID)) {
              log.debug("Try to convert user {} to UUID ...", this.skinName);
              userUUID = PlayersUtils.getUserUUID(serverPlayer.getServer(), this.skinName);
            }
            yield SkinHandler.setPlayerSkin(easyNPC, this.skinName, userUUID);
          }
          case SECURE_REMOTE_URL, INSECURE_REMOTE_URL ->
              SkinHandler.setRemoteSkin(easyNPC, this.skinURL);
          default -> {
            log.error(
                "Failed processing skin:{} uuid:{} url:{} type:{} for {} from {}",
                this.skinName,
                this.skinUUID,
                this.skinURL,
                this.skinType,
                easyNPC,
                serverPlayer);
            yield false;
          }
        };

    if (!successfullyChanged) {
      log.error(
          "Failed changing skin:{} uuid:{} url:{} type:{} for {} from {}",
          this.skinName,
          this.skinUUID,
          this.skinURL,
          this.skinType,
          easyNPC,
          serverPlayer);
    }
  }
}
