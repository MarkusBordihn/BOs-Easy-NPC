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
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.network.message.NetworkMessage;
import io.netty.buffer.Unpooled;
import java.util.UUID;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.phys.Vec3;

public class ChangePositionMessage extends NetworkMessage {

  public static final ResourceLocation MESSAGE_ID =
      new ResourceLocation(Constants.MOD_ID, "change_position");

  protected final Vec3 pos;

  public ChangePositionMessage(final UUID uuid, final float x, final float y, final float z) {
    this(uuid, new Vec3(x, y, z));
  }

  public ChangePositionMessage(final UUID uuid, final Vec3 pos) {
    super(uuid);
    this.pos = pos;
  }

  public static ChangePositionMessage decode(final FriendlyByteBuf buffer) {
    return new ChangePositionMessage(
        buffer.readUUID(), buffer.readFloat(), buffer.readFloat(), buffer.readFloat());
  }

  public static FriendlyByteBuf encode(
      final ChangePositionMessage message, final FriendlyByteBuf buffer) {
    buffer.writeUUID(message.uuid);
    buffer.writeFloat(message.getX());
    buffer.writeFloat(message.getY());
    buffer.writeFloat(message.getZ());
    return buffer;
  }

  public static void handle(final FriendlyByteBuf buffer, final ServerPlayer serverPlayer) {
    handle(decode(buffer), serverPlayer);
  }

  public static void handle(final ChangePositionMessage message, final ServerPlayer serverPlayer) {
    if (!message.handleMessage(serverPlayer)) {
      return;
    }

    // Validate position.
    Vec3 pos = message.getPos();
    if (pos == null) {
      log.error("Invalid pos for {} from {}", message, serverPlayer);
      return;
    }

    // Perform action.
    EasyNPC<?> easyNPC = message.getEasyNPC();
    log.debug("Change pos {} for {} from {}", pos, easyNPC, serverPlayer);
    easyNPC.getEntity().setPos(pos);
  }

  @Override
  public FriendlyByteBuf encode() {
    return encode(this, new FriendlyByteBuf(Unpooled.buffer()));
  }

  public Vec3 getPos() {
    return this.pos;
  }

  public float getX() {
    return (float) this.pos.x;
  }

  public float getY() {
    return (float) this.pos.y;
  }

  public float getZ() {
    return (float) this.pos.z;
  }
}
