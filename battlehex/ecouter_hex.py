import json
from unittest.mock import DEFAULT

import pygame
import math

# Colors
WHITE = (255, 255, 255)
BLUE = (26, 89, 161)

LIGHT_GREEN = (109, 133, 45)
GREEN = (135, 124, 50)
DARK_GREEN = (53, 79, 26)

GREY = (115, 113, 99)
DARK_GREY = (71, 77, 87)
BLACK = (0, 0, 0)

# Hexagon parameters
HEX_SIZE = 45  # Distance from center to vertex
HEX_HEIGHT = HEX_SIZE * math.sqrt(3)  # Vertical distance between centers
HEX_WIDTH = HEX_SIZE * 2  # Horizontal distance between centers
HEX_OFFSET = 1

DEFAULT_TYPE = "grass"

hexes = []

class Game:
    def __init__(
            self,
    ):
        # Initialize Pygame
        pygame.init()
        self.label_font = pygame.font.SysFont('consolas', int(HEX_SIZE * 0.25), bold=True)
        self.description_font = pygame.font.SysFont('calibri light', int(HEX_SIZE * 0.2))

        self.hexes: list[Hex] = [
            hex_
            for hex_ in read_terrain(15, 17)
        ]

    def run(self):
        # Set up the display
        last_hex = self.hexes[-1]

        window = pygame.display.set_mode(
            last_hex.coordinates_xy,
        )
        background = pygame.Surface(window.get_size())

        pygame.display.set_caption("Two Rows of Hexagons")
        pygame.display.init()
        info = pygame.display.Info()

        # # Clear screen
        # window.fill(WHITE)

        for hex in self.hexes:
            hex.draw(
                surface=background,
                label_font=self.label_font,
                description_font=self.description_font,
            )

        # Update display
        window.blit(background, (0, 0))
        pygame.display.flip()

        # Main loop to keep window open
        running = True
        while running:
            for event in pygame.event.get():
                if event.type == pygame.QUIT:
                    running = False

        pygame.quit()


HEX_TYPES = {
    "water": {
        "color": BLUE,
        "description": "",
    },
    "grass": {
        "color": GREEN,
        "description": "",
    },
    "rocksOnGrass": {
        "color": GREY,
        "description": "ТРУДНЫЙ",
    },
    "lightWood": {
        "color": LIGHT_GREEN,
        "description": "РЕДКОЛЕСЬЕ",
    },
    "heavyWood": {
        "color": DARK_GREEN,
        "description": "ГУСТОЙ ЛЕС",
    },
    "pavedRoad": {
        "color": DARK_GREY,
        "description": "",
    },
}


class Hex:
    def __init__(
            self,
            coordinates_xy: tuple[float, float],
            coordinates_jjii: tuple[int, int],
            label: str,
            type: str,
            level: int,
    ):
        self.coordinates_xy = coordinates_xy
        self.coordinates_jjii = coordinates_jjii
        self.label = label
        self.type = type or DEFAULT_TYPE
        self.level = level
        self.size = HEX_SIZE

    # Function to draw a hexagon at (center_x, center_y)
    def draw(self, surface, label_font, description_font):
        points = []
        center_x, center_y = self.coordinates_xy
        for i in range(6):
            angle = math.pi / 3 * i  # 60 degrees in radians
            x = center_x + self.size * math.cos(angle)
            y = center_y + self.size * math.sin(angle)
            points.append((x, y))
        color = HEX_TYPES[self.type]["color"]
        # print(color)
        pygame.draw.polygon(
            surface=surface,
            color=color,
            points=points,
        )
        pygame.draw.polygon(
            surface=surface,
            color=BLACK,
            points=points,
            width=3,
        )

        # Render text
        if self.label:
            text = label_font.render(self.label, True, BLACK)
            text_rect = text.get_rect()
            text_rect.center = (
                center_x, center_y - HEX_HEIGHT / 2 + text_rect.height / 2 + HEX_OFFSET
            )
            surface.blit(text, text_rect)

        # Render type
        description = HEX_TYPES[self.type]["description"]
        if description:
            text = description_font.render(description, True, BLACK)
            text_rect = text.get_rect()
            text_rect.center = (
                center_x, center_y + HEX_HEIGHT / 2 - text_rect.height / 2 - 2 * HEX_OFFSET
            )
            surface.blit(text, text_rect)

        # Render level
        if self.level:
            if self.level > 0:
                level = f"УРОВЕНЬ {self.level}"
            else:
                level = f"ГЛУБИНА {abs(self.level)}"
            text = description_font.render(level, True, BLACK)
            text_rect = text.get_rect()
            if description:
                center_y -= text_rect.height
            text_rect.center = (
                center_x, center_y + HEX_HEIGHT / 2 - text_rect.height / 2 - 2 * HEX_OFFSET
            )
            surface.blit(text, text_rect)



def read_terrain(width, height):
    discovery = read_map()
    for jj in range(width + 2):
        for ii in range(height + 1):  # fill column

            if jj == 0 or jj == width + 1:
                text_label = ""
            else:
                text_label = (str(jj).rjust(2, "0") + str(ii).rjust(2, "0"))
            # print(text_label)

            # Calculate center of each hexagon
            center_x = jj * HEX_WIDTH * 0.75 * HEX_OFFSET
            center_y = ii * HEX_HEIGHT * 1.0 * HEX_OFFSET
            if jj % 2:
                center_y -= HEX_HEIGHT * 0.5 * HEX_OFFSET
                # center_x += HEX_WIDTH * 0.5 * HEX_OFFSET
            # color = BLUE

            record = discovery.get(text_label)

            yield Hex(
                coordinates_xy=(
                    round(center_x, 2),
                    round(center_y, 2),
                ),
                coordinates_jjii=(jj, ii),
                label=text_label,
                type=record["type"] if record else "",
                level=record["level"] if record else 0,
            )


def draw_terrain(size_x, size_y):
    for row in range(size_y * 2 + 3):
        for doubled_column in range(size_x // 2 + 2):

            # Calculate center of each hexagon
            center_x = doubled_column * HEX_WIDTH * 1.5 * HEX_OFFSET
            center_y = -HEX_HEIGHT + row * HEX_HEIGHT * 0.5 * HEX_OFFSET
            if row % 2 == 1:
                center_x += HEX_WIDTH * 0.75 * HEX_OFFSET
            color = BLUE

            if (row, doubled_column) == (35, 7):
                color = GREEN

            # Hex labels
            x_label = 2 * doubled_column + row % 2
            # col_label = row // 2
            y_label = row // 2
            if x_label % 2:
                pass
            else:
                y_label -= 1
            text_label = (
                    str(x_label).rjust(2, "0") + "" +
                    str(y_label).rjust(2, "0")
            )
            # print(text_label)

            draw_hexagon(background, color, center_x, center_y, HEX_SIZE, text_label)


def read_map():
    with open('grassland2.json') as json_file:
        terrain = json.load(json_file)
        # hexes = [
        #     s
        #     for d in terrain
        #     for s in d["hexes"]
        # ]
        # # print(hexes)
        #
        # coords = [
        #     (int(h[:2]) - 1, int(h[2:]) - 1)
        #     for h in hexes
        # ]
        #
        # size_x = max(x for x, _ in coords)
        # size_y = max(y for _, y in coords)

        discovery = dict()

        for t in terrain:
            new_type = t["type"]
            new_level = t["level"]
            for label in t['hexes']:
                if label not in discovery:
                    discovery[label] = {
                        "type": new_type,
                        "level": new_level,
                    }
                    continue
                existing_level = discovery[label]["level"]
                discovery[label]["level"] = max((new_level, existing_level))
                existing_type = discovery[label]["type"]
                if existing_type in ["grass", "water"]:
                    discovery[label]["type"] = new_type
        return discovery

        # for t in terrain:
        #     for label in t['hexes']:
        #         if label not in discovery:
        #             discovery[label] = {
        #                 "type": [],
        #                 "level": [],
        #             }
        #         discovery[label]["type"].append(t["type"])
        #         discovery[label]["level"].append(t["level"])
        #
        # clear_map = {}
        # for label, record in discovery.items():
        #     new_hex = {}
        #     clear_map[label] = new_hex
        #     level, *level_tail = record["level"]
        #     type_, *type_tail = record["type"]
        #     if not level_tail:
        #         new_hex["level"] = level
        #     if not type_tail:
        #         new_hex["type"] = type_
        #
        #     if all(l>=0 for l in record["level"]):
        #         new_hex["level"] = max(l for l in record["level"])
        #     else:
        #         new_hex["level"] = min(l for l in record["level"])
        #
        # return clear_map


if __name__ == "__main__":
    Game().run()