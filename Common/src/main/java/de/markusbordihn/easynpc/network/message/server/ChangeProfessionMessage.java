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
import de.markusbordihn.easynpc.data.profession.Profession;
import de.markusbordihn.easynpc.entity.LivingEntityManager;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.ProfessionData;
import de.markusbordihn.easynpc.network.message.NetworkMessage;
import io.netty.buffer.Unpooled;
import java.util.UUID;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.level.ServerPlayer;

public class ChangeProfessionMessage extends NetworkMessage {

  public static final ResourceLocation MESSAGE_ID =
      new ResourceLocation(Constants.MOD_ID, "change_profession");

  protected final Profession profession;

  public ChangeProfessionMessage(final UUID uuid, final Profession profession) {
    super(uuid);
    this.profession = profession;
  }

  public static ChangeProfessionMessage decode(final FriendlyByteBuf buffer) {
    return new ChangeProfessionMessage(buffer.readUUID(), buffer.readEnum(Profession.class));
  }

  public static FriendlyByteBuf encode(
      final ChangeProfessionMessage message, final FriendlyByteBuf buffer) {
    buffer.writeUUID(message.uuid);
    buffer.writeEnum(message.getProfession());
    return buffer;
  }

  public static void handle(final FriendlyByteBuf buffer, final ServerPlayer serverPlayer) {
    handle(decode(buffer), serverPlayer);
  }

  public static void handle(
      final ChangeProfessionMessage message, final ServerPlayer serverPlayer) {
    if (!message.handleMessage(serverPlayer)) {
      return;
    }

    // Validate name.
    Profession profession = message.getProfession();
    if (profession == null) {
      log.error("Invalid profession for {} from {}", message, serverPlayer);
      return;
    }

    // Validate Profession data.
    EasyNPC<?> easyNPC =
        LivingEntityManager.getEasyNPCEntityByUUID(message.getUUID(), serverPlayer);
    ProfessionData<?> professionData = easyNPC.getEasyNPCProfessionData();
    if (professionData == null) {
      log.error("Invalid profession data for {} from {}", message, serverPlayer);
      return;
    }

    // Perform action.
    log.debug("Change profession {} for {} from {}", profession, easyNPC, serverPlayer);
    professionData.setProfession(profession);
  }

  @Override
  public FriendlyByteBuf encode() {
    return encode(this, new FriendlyByteBuf(Unpooled.buffer()));
  }

  public Profession getProfession() {
    return this.profession;
  }
}
