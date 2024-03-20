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

package de.markusbordihn.easynpc.commands;

import com.mojang.brigadier.Command;
import com.mojang.brigadier.exceptions.CommandSyntaxException;
import de.markusbordihn.easynpc.access.EasyNPCAccessManager;
import de.markusbordihn.easynpc.entity.LivingEntityManager;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.network.NetworkMessageHandler;
import java.util.UUID;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.network.chat.TextComponent;
import net.minecraft.server.level.ServerPlayer;

public class PresetCommandWrapper extends PresetCommand {

  @Override
  protected int exportPreset(CommandSourceStack context, UUID uuid) {
    if (uuid == null) {
      return 0;
    }

    log.info("Try to exporting EasyNPC with UUID {}...", uuid);

    // Check if player is available.
    ServerPlayer serverPlayer;
    try {
      serverPlayer = context.getPlayerOrException();
    } catch (CommandSyntaxException e) {
      context.sendFailure(new TextComponent("This command can only be executed by a player!"));
      return 0;
    }

    // Check if player has access to the EasyNPC.
    if (!EasyNPCAccessManager.hasAccess(context, uuid)) {
      context.sendFailure(new TextComponent("You are not allowed to export this EasyNPC!"));
      return 0;
    }

    // Get the EasyNPC entity by UUID.
    EasyNPC<?> easyNPC = LivingEntityManager.getEasyNPCEntityByUUID(uuid);

    log.info(
        "Exporting EasyNPC {} with UUID {} and skin {}...",
        easyNPC,
        uuid,
        easyNPC.getEasyNPCSkinData().getSkinModel());
    NetworkMessageHandler.exportPresetClient(uuid, serverPlayer);
    context.sendSuccess(
        new TextComponent(
            "Exported EasyNPC "
                + easyNPC.getEntity().getDisplayName().getString()
                + " with UUID "
                + uuid
                + " locally!"),
        true);
    return Command.SINGLE_SUCCESS;
  }
}
