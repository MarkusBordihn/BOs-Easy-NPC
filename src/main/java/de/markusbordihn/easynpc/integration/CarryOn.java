/**
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

package de.markusbordihn.easynpc.integration;

import java.util.List;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import net.minecraftforge.eventbus.api.SubscribeEvent;
import net.minecraftforge.fml.InterModComms;
import net.minecraftforge.fml.ModList;
import net.minecraftforge.fml.common.Mod;
import net.minecraftforge.fml.event.lifecycle.InterModEnqueueEvent;

import de.markusbordihn.easynpc.Constants;

@Mod.EventBusSubscriber(modid = Constants.MOD_ID, bus = Mod.EventBusSubscriber.Bus.MOD)
public class CarryOn {

  public static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  public static final String CARRYON_MOD_ID = "carryon";
  public static final List<String> DENY_LIST = List.of("easy_npc:fairy", "easy_npc:humanoid",
      "easy_npc:humanoid_slim", "easy_npc:skeleton", "easy_npc:villager");

  protected CarryOn() {}

  @SubscribeEvent
  public static void registerDenyList(InterModEnqueueEvent event) {
    // check if carry on is installed before performing any actions.
    if (!ModList.get().isLoaded(CARRYON_MOD_ID)) {
      return;
    }

    log.info("{} CarryOn deny list with {}", Constants.LOG_REGISTER_PREFIX, DENY_LIST);
    for (String entityName : DENY_LIST) {
      InterModComms.sendTo(CARRYON_MOD_ID, "blacklistEntity", () -> entityName);
      InterModComms.sendTo(CARRYON_MOD_ID, "blacklistStacking", () -> entityName);
    }
  }

}
