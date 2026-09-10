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

import de.markusbordihn.easynpc.configui.Constants;
import de.markusbordihn.easynpc.data.sound.SoundDataSet;
import de.markusbordihn.easynpc.data.sound.SoundType;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.SoundDataCapable;
import de.markusbordihn.easynpc.network.message.NetworkMessageRecord;
import de.markusbordihn.easynpc.security.NpcFeature;
import java.util.UUID;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.network.RegistryFriendlyByteBuf;
import net.minecraft.network.codec.StreamCodec;
import net.minecraft.network.protocol.common.custom.CustomPacketPayload;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.level.ServerPlayer;

public record ChangeSoundMessage(
    UUID uuid, SoundType soundType, String soundName, float volume, float pitch, boolean enabled)
    implements NetworkMessageRecord {

  public static final ResourceLocation MESSAGE_ID =
      ResourceLocation.fromNamespaceAndPath(Constants.MOD_ID, "change_sound");
  public static final CustomPacketPayload.Type<ChangeSoundMessage> PAYLOAD_TYPE =
      new Type<>(MESSAGE_ID);
  public static final StreamCodec<RegistryFriendlyByteBuf, ChangeSoundMessage> STREAM_CODEC =
      StreamCodec.of((buffer, message) -> message.write(buffer), ChangeSoundMessage::create);

  public static ChangeSoundMessage create(final FriendlyByteBuf buffer) {
    return new ChangeSoundMessage(
        buffer.readUUID(),
        buffer.readEnum(SoundType.class),
        buffer.readUtf(),
        buffer.readFloat(),
        buffer.readFloat(),
        buffer.readBoolean());
  }

  @Override
  public void write(final FriendlyByteBuf buffer) {
    buffer.writeUUID(this.uuid);
    buffer.writeEnum(this.soundType);
    buffer.writeUtf(this.soundName);
    buffer.writeFloat(this.volume);
    buffer.writeFloat(this.pitch);
    buffer.writeBoolean(this.enabled);
  }

  @Override
  public Type<ChangeSoundMessage> type() {
    return PAYLOAD_TYPE;
  }

  @Override
  public ResourceLocation id() {
    return MESSAGE_ID;
  }

  @Override
  public void handleServer(final ServerPlayer serverPlayer) {
    EasyNPC<?> easyNPC = getEasyNPCAndCheckAccess(this.uuid, serverPlayer);
    if (easyNPC == null) {
      return;
    }

    if (!MessageSecurity.checkFeatureAccess(
        serverPlayer, easyNPC, NpcFeature.SOUND, "sound change")) {
      return;
    }

    SoundDataCapable<?> soundData = easyNPC.getEasyNPCSoundData();
    if (soundData == null || this.soundType == null) {
      log.error("Invalid sound data for {} from {}", easyNPC, serverPlayer);
      return;
    }

    SoundDataSet soundDataSet = new SoundDataSet(soundData.getResolvedSoundDataSet());
    if (this.soundName == null || this.soundName.isEmpty()) {
      log.debug("Reset sound {} for {} from {}", this.soundType, easyNPC, serverPlayer);
      soundDataSet.removeSound(this.soundType);
    } else {
      ResourceLocation soundLocation = ResourceLocation.tryParse(this.soundName);
      if (soundLocation == null) {
        log.error("Invalid sound {} for {} from {}", this.soundName, easyNPC, serverPlayer);
        return;
      }

      log.debug(
          "Change sound {} to {} for {} from {}",
          this.soundType,
          soundLocation,
          easyNPC,
          serverPlayer);
      soundDataSet.addSound(this.soundType, soundLocation, this.volume, this.pitch, this.enabled);
    }

    soundData.setSoundDataSet(soundDataSet);
  }
}
