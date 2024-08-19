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

package de.markusbordihn.easynpc.server.commands;

import com.mojang.brigadier.builder.ArgumentBuilder;
import de.markusbordihn.easynpc.commands.Command;
import de.markusbordihn.easynpc.commands.arguments.EasyNPCArgument;
import de.markusbordihn.easynpc.commands.arguments.EquipmentSlotArgument;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.handler.EquipmentHandler;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.Commands;
import net.minecraft.commands.arguments.item.ItemArgument;
import net.minecraft.world.entity.EquipmentSlot;
import net.minecraft.world.item.Item;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.Items;

public class EquipmentCommand extends Command {

  public static ArgumentBuilder<CommandSourceStack, ?> register() {
    return Commands.literal("equipment")
        .then(
            Commands.literal("set")
                .requires(
                    commandSourceStack -> commandSourceStack.hasPermission(Commands.LEVEL_ALL))
                .then(
                    Commands.argument("target", EasyNPCArgument.npc())
                        .then(
                            Commands.argument("slot", EquipmentSlotArgument.slot())
                                .then(
                                    Commands.argument("item", ItemArgument.item())
                                        .executes(
                                            context ->
                                                setItemSlot(
                                                    context.getSource(),
                                                    EasyNPCArgument.getEntityWithAccess(
                                                        context, "target"),
                                                    EquipmentSlotArgument.getEquipmentSlot(
                                                        context, "slot"),
                                                    ItemArgument.getItem(context, "item")
                                                        .getItem()))))))
        .then(
            Commands.literal("remove")
                .requires(
                    commandSourceStack -> commandSourceStack.hasPermission(Commands.LEVEL_ALL))
                .then(
                    Commands.argument("target", EasyNPCArgument.npc())
                        .then(
                            Commands.argument("slot", EquipmentSlotArgument.slot())
                                .executes(
                                    context ->
                                        removeItemSlot(
                                            context.getSource(),
                                            EasyNPCArgument.getEntityWithAccess(context, "target"),
                                            EquipmentSlotArgument.getEquipmentSlot(
                                                context, "slot"))))));
  }

  private static int setItemSlot(
      CommandSourceStack context, EasyNPC<?> easyNPC, EquipmentSlot equipmentSlot, Item item) {
    if (easyNPC == null || equipmentSlot == null || item == null) {
      return 0;
    }

    // Set item for equipment slot
    ItemStack itemStack = new ItemStack(item);
    if (!EquipmentHandler.setEquipmentSlotItem(easyNPC, equipmentSlot, itemStack)) {
      return sendFailureMessage(
          context,
          easyNPC + " failed to set item stack " + itemStack + " for slot " + equipmentSlot);
    }

    return sendSuccessMessage(
        context, easyNPC + " set item stack" + itemStack + " for slot " + equipmentSlot);
  }

  private static int removeItemSlot(
      CommandSourceStack context, EasyNPC<?> easyNPC, EquipmentSlot equipmentSlot) {
    return setItemSlot(context, easyNPC, equipmentSlot, Items.AIR);
  }
}
