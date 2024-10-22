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

package de.markusbordihn.easynpc.commands.synchronization;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.commands.arguments.DialogArgument;
import de.markusbordihn.easynpc.commands.arguments.EasyNPCArgument;
import de.markusbordihn.easynpc.commands.arguments.EntityTypeArgument;
import de.markusbordihn.easynpc.commands.arguments.EquipmentSlotArgument;
import net.minecraft.commands.synchronization.ArgumentTypes;
import net.minecraft.commands.synchronization.EmptyArgumentSerializer;
import net.minecraft.resources.ResourceLocation;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class ModArgumentTypes {

  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private ModArgumentTypes() {}

  public static void register() {
    ArgumentTypes.register(
        new ResourceLocation(Constants.MOD_ID, "dialog").toString(),
        DialogArgument.class,
        new EmptyArgumentSerializer<>(DialogArgument::new));
    ArgumentTypes.register(
        new ResourceLocation(Constants.MOD_ID, "easy_npc").toString(),
        EasyNPCArgument.class,
        new EmptyArgumentSerializer<>(EasyNPCArgument::new));
    ArgumentTypes.register(
        new ResourceLocation(Constants.MOD_ID, "entity_type").toString(),
        EntityTypeArgument.class,
        new EmptyArgumentSerializer<>(EntityTypeArgument::new));
    ArgumentTypes.register(
        new ResourceLocation(Constants.MOD_ID, "equipment_slot").toString(),
        EquipmentSlotArgument.class,
        new EmptyArgumentSerializer<>(EquipmentSlotArgument::new));
  }
}
