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

package de.markusbordihn.easynpc.network.message.client;

import de.markusbordihn.easynpc.menu.ClientMenuManager;
import de.markusbordihn.easynpc.network.NetworkMessageHandlerManager;
import de.markusbordihn.easynpc.network.message.NetworkMessageRecord;
import java.util.UUID;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.resources.ResourceLocation;
import net.neoforged.neoforge.network.handling.PlayPayloadContext;

public record OpenMenuCallbackMessageRecord(UUID uuid, UUID menuId, CompoundTag data)
    implements NetworkMessageRecord {

  public static OpenMenuCallbackMessageRecord create(FriendlyByteBuf buffer) {
    return new OpenMenuCallbackMessageRecord(
        buffer.readUUID(), buffer.readUUID(), buffer.readNbt());
  }

  @Override
  public void write(FriendlyByteBuf buffer) {
    buffer.writeUUID(this.uuid);
    buffer.writeUUID(this.menuId);
    buffer.writeNbt(this.data);
  }

  @Override
  public ResourceLocation id() {
    return OpenMenuCallbackMessage.MESSAGE_ID;
  }

  public void handle(PlayPayloadContext playPayloadContext) {
    UUID uuid = this.uuid;
    UUID menuId = this.menuId;
    CompoundTag data = this.data;

    ClientMenuManager.setMenuData(menuId, data);
    NetworkMessageHandlerManager.getServerHandler().openMenu(uuid, menuId);
  }
}
