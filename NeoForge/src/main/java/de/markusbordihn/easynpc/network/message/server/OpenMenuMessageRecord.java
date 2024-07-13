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

import de.markusbordihn.easynpc.menu.MenuManager;
import de.markusbordihn.easynpc.network.message.NetworkMessageRecord;
import java.util.UUID;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.resources.ResourceLocation;
import net.neoforged.neoforge.network.handling.PlayPayloadContext;

public record OpenMenuMessageRecord(UUID uuid, UUID menuId) implements NetworkMessageRecord {

  public static OpenMenuMessageRecord create(FriendlyByteBuf friendlyByteBuf) {
    return new OpenMenuMessageRecord(friendlyByteBuf.readUUID(), friendlyByteBuf.readUUID());
  }

  @Override
  public ResourceLocation id() {
    return OpenMenuMessage.MESSAGE_ID;
  }

  @Override
  public void write(FriendlyByteBuf friendlyByteBuf) {
    friendlyByteBuf.writeUUID(this.uuid);
    friendlyByteBuf.writeUUID(this.menuId);
  }

  public void handle(PlayPayloadContext playPayloadContext) {
    playPayloadContext
        .workHandler()
        .submitAsync(
            () -> {
              UUID uuid = this.uuid;
              UUID menuId = this.menuId;
              log.info(
                  "Try open menu message for {} with menuId {} from {}",
                  uuid,
                  menuId,
                  playPayloadContext.player());
              MenuManager.openMenu(menuId);
            });
  }
}
